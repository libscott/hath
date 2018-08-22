{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Hath.Config where

import qualified Data.ByteString.Char8 as BS8

import           Data.Conduit
import           Data.Conduit.Binary

import           Control.Monad.Reader
import           Control.Monad.Trans.Resource

import           Hath.Data.Aeson hiding (Parser)
import           Hath.Prelude

import           Options.Applicative

import           System.Directory
import           System.IO.Unsafe


data HathConfig = HathConfig
  { configPath :: FilePath
  , configVal :: Value
  , configGethEndpoint :: String
  }

newtype GethConfig = GethConfig { gethEndpoint :: String }
  deriving (Show)

instance Has GethConfig HathConfig where
  has = GethConfig . configGethEndpoint


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


optHathConfigPath :: Parser FilePath
optHathConfigPath = strOption
   ( short 'c'
  <> long "config"
  <> help "Path to hath.json"
  <> value "~/.hath/hath.json"
  <> showDefault )

optGethEndpoint :: Parser String
optGethEndpoint = strOption
   ( long "geth"
  <> value "http://localhost:8545"
  <> help "Geth endpoint"
  <> showDefault )

optHathConfig :: Parser HathConfig
optHathConfig = loadConfig <$> optHathConfigPath <*> optGethEndpoint
  where loadConfig path geth = 
          let confVal = unsafePerformIO $ runHath () $ loadJsonConfig path
           in HathConfig path confVal geth

