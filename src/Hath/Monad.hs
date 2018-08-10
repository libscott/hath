{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Hath.Monad where

import qualified Data.ByteString.Char8 as BS8

import           Control.Monad.Reader

import           Hath.Data.Aeson
import           Hath.Prelude

import           System.Directory


data HathConfig = HathConfig
  { getConfigPath :: FilePath
  , gethIpcPath :: FilePath
  } deriving (Show)

runHathConfigured :: Hath HathConfig a -> IO a
runHathConfigured (Hath act) = do
  configPath <- getAppUserDataDirectory "hath"
  gethPath <- (++"/geth.ipc") <$> getAppUserDataDirectory "kmdx"
  let config = HathConfig configPath gethPath
  runStderrLoggingT $ runReaderT act config

loadJsonConfig :: FromJSON a => String -> HathE HathConfig a
loadJsonConfig name = do
  configPath <- getConfigPath <$> ask
  liftIO $ createDirectoryIfMissing False configPath
  let jsonPath = configPath ++ "/" ++ name ++ ".json"
  traceE ("Loading config: " ++ jsonPath) $ do
    exists <- liftIO $ doesFileExist jsonPath
    when (not exists) $ fail "Does not exist"
    ExceptT $ liftIO $ eitherDecodeStrict <$> BS8.readFile jsonPath
