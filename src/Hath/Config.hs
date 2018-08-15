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
loadJsonConfig name = do
  traceE ("Loading config: " ++ name) $ do
    jsonPath <- liftIO $ do
      configPath <- getAppUserDataDirectory "hath"
      createDirectoryIfMissing False configPath
      pure $ configPath ++ "/" ++ name ++ ".json"
    logInfo $ "Loading config: " ++ jsonPath
    Just bs <- liftIO $ runResourceT $ runConduit $ sourceFile jsonPath .| await
    either error pure $ eitherDecodeStrict bs
