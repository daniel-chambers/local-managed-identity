{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
module LMI.Cache
  ( Cache(..)
  , MVarCache
  , newMVarCache
  , NoCache
  , noCache
  ) where

import           Control.Concurrent.MVar (MVar, readMVar, modifyMVar_, newMVar)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

class Cache (cache :: * -> * -> *) where
  readCache :: (MonadIO m, Ord key) => key -> cache key value -> m (Maybe value)
  putCache :: (MonadIO m, Ord key) => key -> value -> cache key value -> m ()


newtype MVarCache key value = MVarCache (MVar (Map key value))

instance Cache MVarCache where
  readCache :: (MonadIO m, Ord key) => key -> MVarCache key value -> m (Maybe value)
  readCache key (MVarCache cacheMVar) = do
    liftIO $ Map.lookup key <$> readMVar cacheMVar

  putCache :: (MonadIO m, Ord key) => key -> value -> MVarCache key value -> m ()
  putCache key value (MVarCache cacheMVar) = do
    liftIO . modifyMVar_ cacheMVar $ \cache ->
      pure $ Map.insert key value cache

newMVarCache :: MonadIO m => m (MVarCache key value)
newMVarCache = liftIO $ MVarCache <$> newMVar Map.empty


newtype NoCache key value = NoCache ()

instance Cache NoCache where
  readCache :: (MonadIO m) => key -> NoCache key value -> m (Maybe value)
  readCache key _ = do
    pure Nothing

  putCache :: (MonadIO m, Ord key) => key -> value -> NoCache key value -> m ()
  putCache key value _ = do
    pure ()

noCache :: NoCache key value
noCache = NoCache ()
