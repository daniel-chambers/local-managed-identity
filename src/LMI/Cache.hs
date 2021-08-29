module LMI.Cache
  ( Cache
  , newCache
  , readCache
  , putCache
  ) where

import           Control.Concurrent.MVar (MVar, readMVar, modifyMVar_, newMVar)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

newtype Cache key value = Cache (MVar (Map key value))

newCache :: MonadIO m => m (Cache key value)
newCache = liftIO $ Cache <$> newMVar Map.empty

readCache :: (MonadIO m, Ord key) => key -> Cache key value -> m (Maybe value)
readCache key (Cache cacheMVar) = do
  liftIO $ Map.lookup key <$> readMVar cacheMVar

putCache :: (MonadIO m, Ord key) => key -> value -> Cache key value -> m ()
putCache key value (Cache cacheMVar) = do
  liftIO . modifyMVar_ cacheMVar $ \cache ->
    pure $ Map.insert key value cache
