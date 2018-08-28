{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Network.Komodo where

import           Data.Serialize
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8 (concat, pack)

import           Network.Bitcoin
import           Hath.Data.Hex
import           Network.Haskoin.Block
import           Network.Haskoin.Constants
import qualified Network.Haskoin.Internals as H

import           Hath.Data.Aeson
import qualified Hath.Data.Binary as Bin
import           Hath.Prelude



-- Notarisation Data ----------------------------------------------------------
--
-- (Doesn't support backnotarisation yet)

data NotarisationData h = NOR
  { blockHash :: h
  , blockNumber :: Word32
  , symbol :: String
  , mom :: h
  , momDepth  :: Word16
  , ccId :: Word16
  } deriving (Eq, Show)

instance Serialize h => Serialize (NotarisationData h) where
  put NOR{..} = do
    put blockHash >> putWord32le blockNumber
    mapM put symbol >> put '\0'
    put mom >> putWord16le momDepth >> putWord16le ccId
  get = do
    let getSymbol =
          get >>= \case '\0' -> pure ""
                        i    -> (i:) <$> getSymbol
    NOR <$> get <*> getWord32le <*> getSymbol
        <*> get <*> getWord16le <*> getWord16le

instance Serialize h => Bin.Binary (NotarisationData h) where
  put = Bin.put . Bin.Ser2Bin
  get = Bin.unSer2Bin <$> Bin.get

-- Notarisation RPC


findNotarisation :: (Serialize h, Has BitcoinConfig r) => Int -> String ->
                    Hath r (Maybe (NotarisationData h))
findNotarisation height symbol = do
  traceE "findNotarisation" $ do
    val <- queryBitcoin "scanNotarisationsDB" [show height, symbol, "10000"]
    pure $ if val == Null
              then Nothing
              else do
                let bs = unHex $ val .! "{opreturn}" :: ByteString
                let Right out = decode bs
                 in Just out

getLastNotarisation :: (Serialize h, Has BitcoinConfig r) => String ->
                       Hath r (Maybe (NotarisationData h))
getLastNotarisation = findNotarisation 0

-- Komodo network settings ----------------------------------------------------

initKomodo :: IO ()
initKomodo = setNetwork komodo

komodo :: Network
komodo = Network
    { getNetworkName = "komodo"
    , getAddrPrefix = 60
    , getScriptPrefix = 85
    , getSecretPrefix = 188

    -- From here down are unverified and unneeded

    , getExtPubKeyPrefix = 4193182861 -- 0x7634504b
    , getExtSecretPrefix = 0x0488ade4
    , getNetworkMagic = 0xf9beb4d9
    , getGenesisHeader =
        BlockHeader
            0x01
            "0000000000000000000000000000000000000000000000000000000000000000"
            "3ba3edfd7a7b12b27ac72c3e67768f617fc81bc3888a51323a9fb8aa4b1e5e4a"
            1231006505
            0x1d00ffff
            2083236893
            -- Hash 000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f
    , getMaxBlockSize = 1000000
    , getMaxSatoshi = 2100000000000000
    , getHaskoinUserAgent = "/hath:0.1.1"
    , getDefaultPort = 8333
    , getAllowMinDifficultyBlocks = False
    , getPowNoRetargetting = False
    , getPowLimit =
        0x00000000ffffffffffffffffffffffffffffffffffffffffffffffffffffffff
    , getBip34Block =
        ( 227931
        , "000000000000024b89b42a942fe0d9fea3bb44ab7bd1b19115dd6a759c0808b8" )
    , getBip65Height = 388381
    , getBip66Height = 363725
    , getTargetTimespan = 14 * 24 * 60 * 60
    , getTargetSpacing = 10 * 60
    , getCheckpoints =
        [ ]
    , getSeeds =
        [ ]
    , getBip44Coin = 0
    }
