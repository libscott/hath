
module Network.Bitcoin where

import qualified Data.Map as Map
import qualified Data.ByteString as BS
import           Data.Attoparsec.ByteString.Char8
import           Data.Scientific

import qualified Hath.Data.Binary as Bin
import           Hath.Data.Aeson hiding (Parser)
import           Hath.Prelude

import qualified Network.Haskoin.Internals as H
import           Network.HTTP.Simple


type BitcoinIdent = (H.PrvKey, H.Address)

data BitcoinConfig =
  BitcoinConfig
    { getUser :: ByteString
    , getPassword :: ByteString
    , getPort :: Int
    } deriving (Show)

instance Has BitcoinConfig BitcoinConfig where
  has = id

loadBitcoinConfig :: FilePath -> Hath r BitcoinConfig
loadBitcoinConfig path = do
  logInfo $ "Loading bitcoin config: " ++ path
  configData <- liftIO $ expandPath path >>= BS.readFile
  let p = \p1 p2 -> parseOnly (parseItem p1 p2) configData
  let econfig = do
        user <- p "rpcuser" $ takeTill (inClass " \n")
        password <- p "rpcpassword" $ takeTill (inClass " \n")
        port <- p "rpcport" decimal
        pure $ BitcoinConfig user password port
  either error pure econfig

 
parseItem :: Parser ByteString -> Parser a -> Parser a
parseItem matchName parseVal = do
  let user = matchName >> skipSpace >> "=" >> skipSpace >> parseVal
      skipLine = takeTill (=='\n') >> endOfLine
  user <|> (skipLine >> parseItem matchName parseVal)

queryBitcoin :: (Has BitcoinConfig r, FromJSON a, ToJSON b) => Text -> b -> Hath r a
queryBitcoin method params = hasReader $ do
  (BitcoinConfig user pass port) <- ask
  let body = "{jsonrpc,method,params,id}" .% (String "2.0", method, toJSON params, Null)
      req = setRequestBasicAuth user pass $ 
            setRequestBodyJSON body $
            setRequestPort port $ "POST http://localhost/"
      interpret v = case v .? "{result}" of
                      Just r  -> pure r
                      Nothing -> error $ "Unexpected response: " ++ asString v
  response <- httpJSONEither req
  traceE ("Bitcoin RPC: " ++ asString body) $
    either (error . show) interpret $ getResponseBody response

bitcoinUtxos :: Has BitcoinConfig r => [H.Address] -> Hath r [BitcoinUtxo]
bitcoinUtxos addrs = queryBitcoin "listunspent" (1::Int, 99999999::Int, addrs)


data BitcoinUtxo = Utxo
  { utxoAmount :: Word64
  , utxoConfirmations :: Int
  , utxoTxid :: H.TxHash
  , utxoVout :: Word32
  , utxoAddress :: H.Address
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
