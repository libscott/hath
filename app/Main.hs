{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as C8L

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map as Map

import           Options.Applicative

import           Network.Ethereum.Crypto
import           Network.Ethereum.Data.Aeson hiding (Parser)
import           Network.Ethereum.Data.RLP
import           Network.Ethereum.Transaction
import           Network.Ethereum.Prelude

import           Network.Hath.Server

import           System.Exit
import           System.IO


type Method = IO Value


jsonArg :: ReadM Value
jsonArg = eitherReader $ eitherDecode . fromString


jsonArgAny :: FromJSON a => ReadM a
jsonArgAny = eitherReader $ eitherDecode . fromString


topMethods :: Parser Method
topMethods = subparser $ etm <> stx <> dtx <> serve <> recover <> txd
  where etm = command "encodeTx" $ info encodeTxMethod (progDesc "encode a json transaction")
        stx = command "signTx" $ info signTxMethod (progDesc "sign a transaction on stdin")
        dtx = command "decodeTx" $ info decodeTxMethod (progDesc "decode a transaction on stdin")
        serve = command "serve" $ info serveMethod (progDesc "run server")
        recover = command "recover" $ info recoverFromMethod (progDesc "recover address")
        txd = command "txid" $ info txidMethod (progDesc "get transaction id")


parseOpts :: ParserInfo Method
parseOpts = info (parser <**> helper) desc
  where
    parser = topMethods
    desc = fullDesc <> progDesc "Ethereum command line utils"


encodeTxMethod :: Parser Method
encodeTxMethod =
  let act tx = (BS8.putStrLn $ B16.encode $ encodeTx tx) >> pure Null
   in act <$> argument jsonArgAny (metavar "JSON TX")


signTxMethod :: Parser Method
signTxMethod = act <$> argument skArg (metavar "secret key")
  where skArg = maybeReader $ secKey . fst . B16.decode . fromString
        act sk = do
          (txBin,_) <- B16.decode <$> BS8.getContents
          let tx = rlpDecode $ rlpDeserialize txBin
              signed = signTx tx sk
          BS8.putStrLn $ B16.encode $ encodeTx signed
          pure Null


decodeTxMethod :: Parser Method
decodeTxMethod = pure $ do
  (txBin,_) <- B16.decode <$> BS8.getContents
  let tx = rlpDecode $ rlpDeserialize txBin :: Transaction
  pure $ toJSON tx


recoverFromMethod :: Parser Method
recoverFromMethod = pure $ do
  (txBin,_) <- B16.decode <$> BS8.getContents
  let tx = rlpDecode $ rlpDeserialize txBin :: Transaction
      toObj pk = object [ "pub" .= toJsonHex (BS.drop 1 (exportPubKey False pk))
                        , "addr" .= pubKeyAddr pk
                        ]
  maybe (fail "Invalid recovery data") (pure . toObj) $ recoverFrom tx


txidMethod :: Parser Method
txidMethod = pure $ do
  (txBin,_) <- B16.decode <$> BS8.getContents
  let tx = rlpDecode $ rlpDeserialize txBin :: Transaction
  pure $ toJSON $ BS8.unpack $ B16.encode $ txid tx


serveMethod :: Parser Method
serveMethod = pure $ serve >> pure Null


main :: IO ()
main = do
  res <- join $ execParser parseOpts
  case res of
     Null -> pure ()
     val -> C8L.putStrLn $ encode val
