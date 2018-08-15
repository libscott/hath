{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Hath.Monad where

import qualified Data.ByteString.Char8 as BS8

import           Data.Conduit
import           Data.Conduit.Binary

import           Control.Monad.Reader
import           Control.Monad.Trans.Resource

import           Hath.Data.Aeson
import           Hath.Prelude

import           System.Directory


runHath :: r -> Hath r a -> IO a
runHath r (Hath act) = runStderrLoggingT $ runReaderT act r

loadJsonConfig :: FromJSON a => String -> HathE r a
loadJsonConfig name = do
  jsonPath <- liftIO $ do
    configPath <- getAppUserDataDirectory "hath"
    createDirectoryIfMissing False configPath
    pure $ configPath ++ "/" ++ name ++ ".json"
  traceE ("Loading config: " ++ jsonPath) $ do
    Just bs <- liftIO $ runResourceT $ runConduit $ sourceFile jsonPath .| await
    liftEither $ eitherDecodeStrict bs
