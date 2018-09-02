{-# LANGUAGE FlexibleInstances #-}

module Network.Bitcoin where

import qualified Data.ByteString as BS
import           Data.Attoparsec.ByteString.Char8
import           Data.Scientific

import qualified Crypto.Secp256k1 as EC

import qualified Hath.Data.Binary as Bin
import           Hath.Data.Aeson hiding (Parser)
import           Hath.Prelude
import           Hath.Prelude.Lifted

import qualified Network.Haskoin.Internals as H
import           Network.HTTP.Simple
import           Network.JsonRpc


type BitcoinIdent = (H.PrvKey, H.PubKey, H.Address)

deriveBitcoinIdent :: EC.SecKey -> BitcoinIdent
deriveBitcoinIdent sk =
  let bitcoinKey = H.makePrvKey sk
      pubKey = H.derivePubKey bitcoinKey
   in (bitcoinKey, pubKey, H.pubKeyAddr pubKey)

data BitcoinConfig =
  BitcoinConfig
    { getUser :: ByteString
    , getPassword :: ByteString
    , getPort :: Int
    } deriving (Show)


loadBitcoinConfig :: FilePath -> IO BitcoinConfig
loadBitcoinConfig path = do
  runHath () $ logInfo $ "Loading bitcoin config: " ++ path
  configData <- liftIO $ expandPath path >>= BS.readFile
  let p = \p1 p2 -> parseOnly (parseItem p1 p2) configData
  let econfig = do
        user <- p "rpcuser" $ takeTill (inClass " \n")
        password <- p "rpcpassword" $ takeTill (inClass " \n")
        port <- either (const $ pure 7771) pure $ p "rpcport" decimal
        pure $ BitcoinConfig user password port
  case econfig of
    Left _ -> error $ "Check config at : " ++ path
    Right c -> pure c
 
parseItem :: Parser ByteString -> Parser a -> Parser a
parseItem matchName parseVal = do
  let user = matchName >> skipSpace >> "=" >> skipSpace >> parseVal
      skipLine = takeTill (=='\n') >> endOfLine
  user <|> (skipLine >> parseItem matchName parseVal)

queryBitcoin :: (Has BitcoinConfig r, FromJSON a, ToJSON b) => Text -> b -> Hath r a
queryBitcoin method params = hasReader $ do
  (BitcoinConfig user pass port) <- ask
  let endpoint =
        HttpEndpoint $
          setRequestBasicAuth user pass $ 
          setRequestPort port "http://localhost/"
  queryJsonRpc endpoint method params

bitcoinSubmitTxSync :: Has BitcoinConfig r => H.Tx -> Hath r H.TxHash
bitcoinSubmitTxSync tx = do
  txid <- queryBitcoin "sendrawtransaction" [tx]
  let wait height = do
        block <- queryBitcoin "getblock" [show height]
        if elem txid $ (block .! "{tx}" :: [H.TxHash])
           then pure txid
           else fix $ \f -> do
             height' <- bitcoinGetHeight
             if height' /= height
                then wait $ height + 1
                else threadDelay 5000000 >> f
  bitcoinGetHeight >>= wait


bitcoinGetHeight :: Has BitcoinConfig r => Hath r Int
bitcoinGetHeight = queryBitcoin "getinfo" () <&> (.!"{blocks}")

-- UTXOs ----------------------------------------------------------------------

bitcoinUtxos :: Has BitcoinConfig r => [H.Address] -> Hath r [BitcoinUtxo]
bitcoinUtxos addrs = queryBitcoin "listunspent" (1::Int, 99999999::Int, addrs)

data BitcoinUtxo = Utxo
  { utxoAmount :: Word64
  , utxoConfirmations :: Int
  , utxoTxid :: H.TxHash
  , utxoVout :: Word32
  , utxoAddress :: H.Address
  , utxoSpendable :: Bool
  } deriving (Show)

instance FromJSON BitcoinUtxo where
  parseJSON val = do
    obj <- parseJSON val
    amount <- obj .: "amount"
    Utxo (floor $ amount * (1e8::Scientific))
         <$> obj .: "confirmations"
         <*> obj .: "txid"
         <*> obj .: "vout"
         <*> obj .: "address"
         <*> obj .: "spendable"

getOutPoint :: BitcoinUtxo -> H.OutPoint
getOutPoint utxo = H.OutPoint (utxoTxid utxo) (utxoVout utxo)


-- Instances ------------------------------------------------------------------

instance Bin.Binary H.TxIn where
  put = Bin.put . Bin.Ser2Bin 
  get = Bin.unSer2Bin <$> Bin.get

instance Bin.Binary H.Address where
  put = Bin.put . Bin.Ser2Bin
  get = Bin.unSer2Bin <$> Bin.get

instance Bin.Binary H.OutPoint where
  put = Bin.put . Bin.Ser2Bin
  get = Bin.unSer2Bin <$> Bin.get

instance Bin.Binary H.PubKey where
  put = Bin.put . Bin.Ser2Bin
  get = Bin.unSer2Bin <$> Bin.get
