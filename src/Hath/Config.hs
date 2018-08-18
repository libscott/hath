{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Hath.Config where

import qualified Data.ByteString.Char8 as BS8

import           Data.Conduit
import           Data.Conduit.Binary

import           Control.Monad.Reader
import           Control.Monad.Trans.Resource

import           Hath.Data.Aeson
import           Hath.Prelude

import           System.Directory


loadJsonConfig :: FromJSON a => String -> Hath r a
loadJsonConfig path = do
  traceE ("Loading config: " ++ path) $ do
    jsonPath <- liftIO $ do
      case path of
        ('~':xs) -> (++xs) <$> getHomeDirectory
        _        -> pure path
    logInfo $ "Loading config: " ++ jsonPath
    Just bs <- liftIO $ runResourceT $ runConduit $ sourceFile jsonPath .| await
    either error pure $ eitherDecodeStrict bs
