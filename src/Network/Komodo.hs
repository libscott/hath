{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Network.Komodo where

import           Data.Serialize

import           Network.Bitcoin
import           Hath.Data.Hex
import           Network.Haskoin.Block
import           Network.Haskoin.Constants
import           Network.Haskoin.Transaction

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

momDepth32 :: Integral a => NotarisationData h -> a
momDepth32 = fromIntegral . momDepth

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

instance Serialize h => FromJSON (NotarisationData h) where
  parseJSON val = do
    Hex bs <- parseJSON val
    either fail pure $ decode bs

-- Notarisation RPC

data Notarisation h = Notarisation
  { kmdHeight :: Word32
  , txHash :: TxHash
  , opret :: NotarisationData h
  } deriving (Show)

instance Serialize h => FromJSON (Notarisation h) where
  parseJSON val = do
    obj <- parseJSON val
    Notarisation <$> obj .: "height"
                 <*> obj .: "hash"
                 <*> obj .: "opreturn"

scanNotarisationsDB :: (Serialize h, Has BitcoinConfig r) => Word32 -> String ->
                    Word32 -> Hath r (Maybe (Notarisation h))
scanNotarisationsDB height symbol limit = do
  traceE "scanNotarisationsDB" $ do
    val <- queryBitcoin "scanNotarisationsDB" [show height, symbol, show limit]
    pure $ if val == Null
              then Nothing
              else Just $ val .! "."

getLastNotarisation :: (Serialize h, Has BitcoinConfig r) => String ->
                       Hath r (Maybe (Notarisation h))
getLastNotarisation s = scanNotarisationsDB 0 s 10000

-- Komodo network settings ----------------------------------------------------

initKomodo :: IO ()
initKomodo = setNetwork komodo

komodo :: Network
komodo = prodnet
    { getNetworkName = "komodo"
    , getAddrPrefix = 60
    , getScriptPrefix = 85
    , getSecretPrefix = 188
    }
