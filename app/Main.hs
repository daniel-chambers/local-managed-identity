module Main where

import           Prelude hiding (putStrLn)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.IO (putStrLn)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import           Data.Version (showVersion)
import           LMI.WebApi (ApiSettings(..), runWebApi, Port(..), SecretValue(..))
import           Options.Applicative (Parser, auto, help, hidden, info, infoOption, long, metavar, option, strOption, progDesc, short, value, execParser, optional, switch, helper, showDefault)
import           System.Log.FastLogger (LogType'(..), newTimeCache, simpleTimeFormat, withTimedFastLogger, defaultBufSize, ToLogStr (toLogStr))
import qualified Paths_local_managed_identity as PackageInfo

data CommandLineArguments =
  CommandLineArguments { _claPort :: Port
                       , _claSecret :: Maybe Text
                       , _claCacheTokens :: Bool }

commandLineArgumentsParser :: Parser CommandLineArguments
commandLineArgumentsParser =
  CommandLineArguments
  <$> option auto
    ( long "port"
      <> short 'p'
      <> help "The port the server will run on"
      <> value 5436
      <> showDefault
      <> metavar "PORT" )
  <*> optional (strOption
    ( long "secret"
      <> short 's'
      <> help "The required value of the 'secret' header that must be sent by the client in its requests. If omitted, a random GUID will be used."
      <> metavar "VALUE" ))
  <*> switch
    ( long "cache-tokens"
      <> help "Enables in-memory caching of tokens until just before expiry. Without caching Azure CLI is invoked on every request."
      <> short 'c' )

version :: Parser (a -> a)
version =
  infoOption displayText
    ( long "version"
      <> short 'v'
      <> help "Prints the version of the application and quits"
      <> hidden )
  where displayText = "Version " <> showVersion PackageInfo.version

readArguments :: IO CommandLineArguments
readArguments =
  execParser $ info (helper <*> version <*> commandLineArgumentsParser) argsInfo
  where argsInfo = progDesc "Local Managed Identity"

main :: IO ()
main = do
  timeCache <- newTimeCache "%Y-%m-%d %H:%M:%S"
  CommandLineArguments{..} <- readArguments
  secret <- case _claSecret of
    Just secret -> pure secret
    Nothing -> UUID.toText <$> UUID.nextRandom

  withTimedFastLogger timeCache (LogStdout defaultBufSize) $ \timedFastLogger -> do
    let fastLogger logStr = timedFastLogger (\time -> "[" <> toLogStr time <> "] " <> logStr <> "\n")

    putStrLn $ Text.concat [ "Local Managed Identity v" <> Text.pack (showVersion PackageInfo.version) ]
    putStrLn "---- PowerShell Environment Variables ----"
    putStrLn $ Text.concat [ "$env:MSI_ENDPOINT = \"http://localhost:", showt _claPort, "/\"" ]
    putStrLn $ Text.concat [ "$env:MSI_SECRET = \"", secret ,"\"" ]
    putStrLn "------- Bash Environment Variables ------"
    putStrLn $ Text.concat [ "export MSI_ENDPOINT=\"http://localhost:", showt _claPort, "/\"" ]
    putStrLn $ Text.concat [ "export MSI_SECRET=\"", secret ,"\"" ]
    putStrLn "-- Docker (Windows/Mac) Run Parameters --"
    putStrLn $ Text.concat [ "-e MSI_ENDPOINT=http://host.docker.internal:", showt _claPort, "/ -e MSI_SECRET" ]
    putStrLn "-----------------------------------------"
    putStrLn "Server started. Ctrl+C to quit."

    runWebApi fastLogger (ApiSettings _claPort (SecretValue secret) _claCacheTokens)

showt :: Show a => a -> Text
showt = Text.pack . show
