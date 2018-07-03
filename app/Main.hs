{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as C8L

import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map as Map

import           Options.Applicative

import qualified Network.Ethereum.API as API
import           Network.Ethereum.API.Tx
import           Network.Ethereum.Crypto
import           Network.Ethereum.Data.Aeson hiding (Parser)
import           Network.Ethereum.Transaction
import           Network.Ethereum.Prelude

import           Network.Hath.Server

import           System.Exit
import           System.IO


type Method = ExceptT Err IO Value


parseCmd :: Parser Method
parseCmd = subparser $
  foldl1 (<>) $ (\(c,(_,h)) -> apiMethod c h) <$> methods
  where
    methods = Map.toList API.methods
    apiMethod c h = command c $ info (parseMethod c) (progDesc h)
    parseMethod c = API.runMethod c <$> argument jsonArg (metavar "JSON")


jsonArg :: ReadM Value
jsonArg = eitherReader $ eitherDecode . fromString


jsonArgAny :: FromJSON a => ReadM a
jsonArgAny = eitherReader $ eitherDecode . fromString


topMethods :: Parser Method
topMethods = subparser $ etm <> jms <> stx <> dtx <> serve <> recover <> txd
  where etm = command "encodeTx" $ info encodeTxMethod (progDesc "encode a json transaction")
        jms = command "json" $ info parseCmd (progDesc "json api")
        stx = command "signTx" $ info signTxMethod (progDesc "sign a transaction on stdin")
        dtx = command "decodeTx" $ info decodeTxMethod (progDesc "decode a transaction on stdin")
        serve = command "serve" $ info serveMethod (progDesc "run server")
        recover = command "recover" $ info recoverFromMethod (progDesc "recover address")
        txd = command "txid" $ info txidMethod (progDesc "get transaction id")


parseOpts :: ParserInfo (Bool, Method)
parseOpts = info (parser <**> helper) desc
  where
    parser = (,) <$> pretty <*> topMethods
    pretty = switch (long "pretty" <> help "Pretty print output")
    desc = fullDesc <> progDesc "Ethereum command line utils"


encodeTxMethod :: Parser Method
encodeTxMethod =
  let act tx = (lift $ BS8.putStrLn $ encodeTxHex tx) >> pure Null
   in act <$> argument jsonArgAny (metavar "JSON TX")


signTxMethod :: Parser Method
signTxMethod = act <$> argument skArg (metavar "secret key")
  where skArg = maybeReader $ secKey . fst . B16.decode . fromString
        act sk = do
          (txBin,_) <- B16.decode <$> lift BS8.getContents
          let tx = rlpDecode $ rlpDeserialize txBin
              signed = signTx tx sk
          lift $ BS8.putStrLn $ encodeTxHex signed
          pure Null


decodeTxMethod :: Parser Method
decodeTxMethod = pure $ do
  (txBin,_) <- B16.decode <$> lift BS8.getContents
  let tx = rlpDecode $ rlpDeserialize txBin :: Transaction
  pure $ toJSON tx


recoverFromMethod :: Parser Method
recoverFromMethod = pure $ do
  (txBin,_) <- B16.decode <$> lift BS8.getContents
  let tx = rlpDecode $ rlpDeserialize txBin :: Transaction
      toObj pk = object [ "pub" .= toJsonHex (BS.drop 1 (exportPubKey False pk))
                        , "addr" .= pubKeyAddr pk
                        ]
  pure $ toJSON $ toObj <$> recoverFrom tx


txidMethod :: Parser Method
txidMethod = pure $ do
  (txBin,_) <- B16.decode <$> lift BS8.getContents
  let tx = rlpDecode $ rlpDeserialize txBin :: Transaction
  pure $ toJSON $ BS8.unpack $ B16.encode $ txid tx


serveMethod :: Parser Method
serveMethod = pure $ lift serve >> pure Null


main :: IO ()
main = do
  (pretty, act) <- execParser parseOpts
  res <- runExceptT act
  case res of
       Left err -> hPutStrLn stderr (show err) >> exitFailure
       Right Null -> pure ()
       Right val -> do
          let enc = if pretty then encodePretty' pconf else encode
          C8L.putStrLn $ enc val
  where
    pconf = defConfig { confCompare=compare, confIndent=Spaces 2 }
