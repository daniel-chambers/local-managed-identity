module LMI.AzureCli
  ( AccessTokenParams(..)
  , AccessToken(..)
  , AccessTokenError(..)
  , Error(..)
  , getAccessToken
  ) where

import Data.Aeson (FromJSON(..), eitherDecodeStrict, withObject, (.:), withText)
import Data.Bifunctor (bimap)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.Thyme (LocalTime, parseTime, UTCTime, TimeZone, getCurrentTimeZone)
import Data.Thyme.Time (localTimeToUTC)
import Data.Thyme.Format.Aeson ()
import System.Exit (ExitCode (ExitSuccess))
import System.Process.ByteString (readCreateProcessWithExitCode)
import System.Process.ListLike (CreateProcess, shell)
import System.Locale (defaultTimeLocale)

newtype LocalTimeFormat = LocalTimeFormat LocalTime

unLocalTimeFormat :: LocalTimeFormat -> LocalTime
unLocalTimeFormat (LocalTimeFormat lt) = lt

instance FromJSON LocalTimeFormat where
  parseJSON = withText "LocalTimeFormat" (\text ->
    case parseTime defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q" (unpack text) of
      Just t -> pure $ LocalTimeFormat t
      Nothing -> fail "Could not parse local date format"
    )

data AccessTokenParams =
  AccessTokenParams { _atpResource :: Maybe Text
                    , _atpSubscriptionId :: Maybe Text
                    , _atpTenantId :: Maybe Text }

data CliAccessToken =
  CliAccessToken { _catAccessToken :: Text
                 , _catExpiresOn :: LocalTimeFormat
                 , _catSubscriptionId :: Text
                 , _catTenantId :: Text
                 , _catTokenType :: Text }

data AccessToken =
  AccessToken { _atAccessToken :: Text
              , _atExpiresOn :: UTCTime
              , _atSubscriptionId :: Text
              , _atTenantId :: Text
              , _atTokenType :: Text }

instance FromJSON CliAccessToken where
  parseJSON = withObject "AccessToken" $ \v ->
    CliAccessToken
    <$> v .: "accessToken"
    <*> v .: "expiresOn"
    <*> v .: "subscription"
    <*> v .: "tenant"
    <*> v .: "tokenType"

data AccessTokenError =
    JsonError Error
  | CliError Error ExitCode

newtype Error = Error Text

toAccessToken :: TimeZone -> CliAccessToken -> AccessToken
toAccessToken localTimezone CliAccessToken{..} =
  AccessToken _catAccessToken utcExpiresOn _catSubscriptionId _catTenantId _catTokenType
  where
    utcExpiresOn = localTimeToUTC localTimezone (unLocalTimeFormat _catExpiresOn)

buildCommand :: AccessTokenParams -> CreateProcess
buildCommand AccessTokenParams{..} =
  shell . unwords $ concat [ baseCommand, resourceParam, subscriptionParam, tenantParam ]
  where
    baseCommand = ["az", "account", "get-access-token" ]
    resourceParam = maybe [] (\r -> ["--resource", unpack r]) _atpResource
    subscriptionParam = maybe [] (\s -> ["-s", unpack s]) _atpSubscriptionId
    tenantParam = maybe [] (\t -> ["-t", unpack t]) _atpTenantId

getAccessToken :: AccessTokenParams -> IO (Either AccessTokenError AccessToken)
getAccessToken params = do
  localTimezone <- getCurrentTimeZone
  (exitCode, stdout, stderr) <- readCreateProcessWithExitCode command ""
  if exitCode == ExitSuccess then
    pure . bimap (JsonError . Error . pack) (toAccessToken localTimezone) $ eitherDecodeStrict stdout
  else
    pure . Left $ CliError (Error $ decodeUtf8 stderr) exitCode
  where
    command = buildCommand params
