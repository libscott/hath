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
  } deriving (Show)


getHathConfig :: IO HathConfig
getHathConfig =
  let configPath = getAppUserDataDirectory "hath"
   in HathConfig <$> configPath

runHath :: r -> Hath r a -> IO a
runHath r (Hath act) = runStderrLoggingT $ runReaderT act r

-- TODO: remove
runHathConfigured act = getHathConfig >>= flip runHath act

loadJsonConfig :: FromJSON a => String -> HathE HathConfig a
loadJsonConfig name = do
  configPath <- getConfigPath <$> ask
  liftIO $ createDirectoryIfMissing False configPath
  let jsonPath = configPath ++ "/" ++ name ++ ".json"
  traceE ("Loading config: " ++ jsonPath) $ do
    exists <- liftIO $ doesFileExist jsonPath
    when (not exists) $ fail "Does not exist"
    ExceptT $ liftIO $ eitherDecodeStrict <$> BS8.readFile jsonPath
