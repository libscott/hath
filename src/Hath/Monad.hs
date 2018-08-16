{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Hath.Monad where

import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader


newtype Hath r a = Hath (ReaderT r (LoggingT IO) a)
  deriving (MonadReader r)

instance Functor (Hath r) where
  fmap f (Hath a) = Hath $ fmap f a

instance Applicative (Hath r) where
  pure a = Hath $ pure a
  (Hath f) <*> (Hath a) = Hath $ f <*> a

instance Monad (Hath r) where
  (Hath a) >>= f = Hath $ a >>= unHath . f

instance MonadIO (Hath r) where
  liftIO a = Hath $ liftIO a

instance MonadLogger (Hath r) where
  monadLoggerLog a b c d = Hath $ monadLoggerLog a b c d

runHath :: r -> Hath r a -> IO a
runHath r (Hath act) = runStderrLoggingT $ runReaderT act r

unHath :: Hath r a -> ReaderT r (LoggingT IO) a
unHath (Hath a) = a

hathReader :: (r -> r') -> Hath r' a -> Hath r a
hathReader f = Hath . withReaderT f . unHath

-- The Has type
class Has r a where
  has :: a -> r

instance Has r r where
  has = id

hasReader :: Has r' r => Hath r' a -> Hath r a
hasReader = hathReader has
