{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Hath.Config where

import           Control.Applicative

import qualified Data.ByteString.Char8 as BS8

import           Network.Ethereum.Crypto

import           Hath.Data.Aeson hiding (Parser)
import           Hath.Prelude

import           Options.Applicative


newtype GethConfig = GethConfig { gethEndpoint :: String }
  deriving (Show)

optGethConfig :: Parser GethConfig
optGethConfig = GethConfig <$> strOption
   ( long "geth"
  <> value "http://localhost:8545"
  <> help "Geth endpoint"
  <> metavar "URL"
  <> showDefault )

optMandate :: Parser Address
optMandate = option auto
   ( long "mandate"
  <> metavar "ADDRESS"
  <> help "Mandate contract address 0x..." )

data ConsensusNetworkConfig = CNC
  { host :: String
  , port :: Word16
  , seeds :: [String]
  }

consensusDefaultPort :: Word16
consensusDefaultPort = 40440

optHost = strOption
   ( long "host"
  <> metavar "IP"
  <> help "Public IP to bind to" )

optPort = option auto
   ( long "port"
  <> metavar "NUM"
  <> value consensusDefaultPort
  <> showDefault
  <> help "Port to bind to" )

optSeeds = strOption
   ( long "seed"
  <> metavar "HOST"
  <> help "ip[:port]" )

optConsensusConfig = CNC <$> optHost <*> optPort <*> some optSeeds

-- Helpers --------------------------------------------------------------------

jsonArg :: FromJSON a => ReadM a
jsonArg = eitherReader $ eitherDecode . fromString

optJsonOrStdin :: FromJSON a => Mod ArgumentFields (IO a) -> Parser (IO a)
optJsonOrStdin props =
  let d = eitherDecodeStrict
      f = eitherReader $ \s -> pure <$> d (fromString s)
      f' = either error id . d <$> BS8.getLine
   in argument f $ (value f') <> props
