{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Hath.Monad where

import           Control.Concurrent
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader

import qualified Data.Map as Map


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

-- Parallel computation

parM :: Int -> [a] -> (a -> Hath r b) -> Hath r [b]
parM initialSlots items act = do
  r <- ask
  mvar <- liftIO $ newEmptyMVar
  
  let fork (i,o) = do
        forkIO $ do
          res <- runHath r (act o)
          putMVar mvar (i, res)

  let run slots rmap rest
        | slots == initialSlots && null rest = do
            pure $ snd <$> Map.toAscList rmap
        | slots > 0 && not (null rest) = do
            let (j:xs) = rest
            fork j
            run (slots-1) rmap xs
        | otherwise = do
            (i, res) <- takeMVar mvar
            let nmap = Map.insert i res rmap
            run (slots+1) nmap rest

  liftIO $ run initialSlots Map.empty $ zip [0..] items


