module Hath.Lifted
  ( IN.MVar(..)
  , readMVar
  , modifyMVar_
  , threadDelay
  , newMVar
  , newEmptyMVar
  , takeMVar
  , putMVar
  ) where

import qualified Control.Concurrent as IN
import           Control.Monad.IO.Class


threadDelay :: MonadIO m => Int -> m ()
threadDelay = liftIO . IN.threadDelay

newMVar :: MonadIO m => a -> m (IN.MVar a)
newMVar = liftIO . IN.newMVar

newEmptyMVar :: MonadIO m => m (IN.MVar a)
newEmptyMVar = liftIO IN.newEmptyMVar

readMVar :: MonadIO m => IN.MVar a -> m a
readMVar = liftIO . IN.readMVar

modifyMVar_ :: MonadIO m => IN.MVar a -> (a -> IO a) -> m () 
modifyMVar_ mv = liftIO . IN.modifyMVar_ mv

takeMVar :: MonadIO m => IN.MVar a -> m a
takeMVar = liftIO . IN.takeMVar

putMVar :: MonadIO m => IN.MVar a -> a -> m ()
putMVar mv = liftIO . IN.putMVar mv
