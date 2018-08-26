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
  <> showDefault )

optMandate :: Parser Address
optMandate = option auto
   ( long "mandate"
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
  <> value "0.0.0.0"
  <> showDefault
  <> help "host to bind to" )

optPort = option auto
   ( long "port"
  <> value consensusDefaultPort
  <> showDefault
  <> help "port to bind to" )

optSeeds = strOption
   ( long "seed"
  <> help "ip[:port]" )

optConsensusConfig = CNC <$> optHost <*> optPort <*> some optSeeds
