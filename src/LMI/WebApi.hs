module LMI.WebApi
  ( runWebApi
  , SecretValue(..)
  , Port
  ) where

import           Control.Applicative ((*>))
import           Control.Concurrent.MVar (MVar, readMVar, modifyMVar_, newMVar)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson (ToJSON(..), pairs, object, (.=))
import           Data.AffineSpace ((.-^))
import           Data.Text (Text)
import qualified Data.Text.IO as Text
import           Data.Text.Lazy (toStrict)
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Data.Thyme.Time (UTCTime, getCurrentTime, fromSeconds)
import           GHC.Base (join)
import           LMI.AzureCli (AccessToken(..), AccessTokenParams(..), AccessTokenError(..), Error(..))
import qualified LMI.AzureCli as AzureCli
import           LMI.Cache (Cache, readCache, putCache, newCache)
import           Network.HTTP.Types.Status (status401, status500)
import           Network.Wai (Request, queryString)
import           Network.Wai.Handler.Warp (Port, defaultSettings, setPort)
import           System.Log.FastLogger (FastLogger, toLogStr)
import           Web.Scotty (ScottyM, ActionM, Options(..), scottyOpts, get, header, param, request, json, status, notFound)

data AccessTokenResponse =
    AccessTokenResponse { _atrAccessToken :: Text
                        , _atrExpiresOn :: UTCTime }

instance ToJSON AccessTokenResponse where
  toJSON AccessTokenResponse{..} =
    object [
      "access_token" .= _atrAccessToken,
      "expires_on" .= _atrExpiresOn
    ]

  toEncoding AccessTokenResponse{..} =
    pairs (
      "access_token" .= _atrAccessToken
      <> "expires_on" .= _atrExpiresOn
    )

newtype ErrorResponse = ErrorResponse Text

instance ToJSON ErrorResponse where
  toJSON (ErrorResponse err) =
    object [ "error" .= err ]

  toEncoding (ErrorResponse err) =
    pairs ("error" .= err)

newtype SecretValue = SecretValue Text

runWebApi :: FastLogger -> Port -> SecretValue -> IO ()
runWebApi log port secretValue = do
  accessTokenCache <- newCache
  scottyOpts scottyOptions (api log secretValue accessTokenCache)
  where
    scottyOptions = Options silent settings
    silent = 0
    settings = setPort port defaultSettings

api :: FastLogger -> SecretValue -> Cache (Maybe Resource) AccessToken -> ScottyM ()
api log expectedSecret accessTokenCache = do
  get "/" (getAccessTokenRoute log expectedSecret accessTokenCache)


getAccessTokenRoute :: FastLogger -> SecretValue -> Cache (Maybe Resource) AccessToken -> ActionM ()
getAccessTokenRoute log (SecretValue expectedSecret) accessTokenCache = do
  secret <- fmap toStrict <$> header "secret"
  resource <- lookupQueryParam "resource" <$> request
  if secret == Just expectedSecret then do
    cachedAccessToken <- tryAccessTokenCache resource accessTokenCache
    case cachedAccessToken of
      Just AccessToken{..} -> do
        logText $ case resource of
          Just r -> "Returned cached token for resource " <> r
          Nothing -> "Returned cached token for default resource (ARM)"
        json $ AccessTokenResponse _atAccessToken _atExpiresOn

      Nothing -> do
        tokenResult <- liftIO $ AzureCli.getAccessToken (AccessTokenParams resource Nothing Nothing)
        case tokenResult of
          Left (JsonError (Error err)) -> do
            logText $ "Failed to deserialize request from Azure CLI: " <> err
            status status500
            json (ErrorResponse err)

          Left (CliError (Error err) _) -> do
            logText $ "Failed to invoke Azure CLI: " <> err
            status status500
            json (ErrorResponse err)

          Right accessToken@AccessToken{..} -> do
            logText $ case resource of
              Just r -> "Returned token for resource " <> r
              Nothing -> "Returned token for default resource (ARM)"

            putCache resource accessToken accessTokenCache
            json $ AccessTokenResponse _atAccessToken _atExpiresOn
  else do
    logText "Unauthenticated request received (incorrect secret header)"
    status status401
    json (ErrorResponse "Invalid secret header")
  where
    logText :: MonadIO m => Text -> m ()
    logText = liftIO . log . toLogStr

tryAccessTokenCache :: MonadIO m => Maybe Resource -> Cache (Maybe Resource) AccessToken -> m (Maybe AccessToken)
tryAccessTokenCache resource cache = do
  cachedAccessToken <- readCache resource cache
  case cachedAccessToken of
    Just accessToken@AccessToken{..} -> do
      currentTime <- liftIO getCurrentTime
      -- If it's going to expired in a minute, let's just get a new token
      if _atExpiresOn .-^ fromSeconds 60 <= currentTime then
        pure Nothing
      else
        pure $ Just accessToken
    Nothing ->
      pure Nothing



lookupQueryParam :: Text -> Request -> Maybe Text
lookupQueryParam name request =
  decodeUtf8 <$> join (lookup nameBS params)
  where
    params = queryString request
    nameBS = encodeUtf8 name


type Resource = Text
