{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Network.Hath.Monad.Types where

import           Control.Monad.Fail
import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader
import qualified Control.Monad.Trans.Class as Trans
import Debug.Trace


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

unHath :: Hath r a -> ReaderT r (LoggingT IO) a
unHath (Hath a) = a

-- The HathE type
--
type HathE r = ExceptT String (Hath r)

instance MonadFail (ExceptT String (Hath r)) where
  fail = throwError

hathReader :: (r -> r') -> HathE r' a -> HathE r a
hathReader f = ExceptT . Hath . withReaderT f . unHath . runExceptT

-- The Has type
class Has r a where
  has :: a -> r

instance Has r r where
  has = id

hasReader :: Has r' r => HathE r' a -> HathE r a
hasReader = hathReader has
