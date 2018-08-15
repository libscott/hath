{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Network.Bitcoin where


import qualified Data.Map as Map
import qualified Data.ByteString as BS
import           Data.Attoparsec.ByteString.Char8

import           Hath.Data.Aeson hiding (Parser)
import           Hath.Prelude
import           Network.HTTP.Simple


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

queryBitcoin :: (Has BitcoinConfig r, FromJSON a) => Text -> [Value] -> Hath r a
queryBitcoin method params = hasReader $ do
  (BitcoinConfig user pass port) <- ask
  let body = "{jsonrpc,method,params,id}" .% (String "2.0", method, params, Null)
      req = setRequestBasicAuth user pass $ 
            setRequestBodyJSON body $
            setRequestPort port $ "POST http://localhost/"
      interpret v = case v .? "{result}" of
                      Just r  -> pure r
                      Nothing -> error $ "Unexpected response: " ++ asString v
  response <- httpJSONEither req
  traceE ("Bitcoin RPC: " ++ asString body) $
    either (error . show) interpret $ getResponseBody response
