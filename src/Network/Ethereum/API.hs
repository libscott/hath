{-# LANGUAGE OverloadedStrings #-}

module Network.Ethereum.API
  ( module API
  , methods
  , runJsonRpc
  , runMethod
  , wrapJson
  ) where

import           Data.Aeson.Types
import qualified Data.Map as Map

import           Lens.Micro

import           Network.Ethereum.API.Utils as API
import           Network.Ethereum.API.Tx as API
import           Network.Ethereum.Prelude


methods :: Map.Map String (JsonMethod, String)
methods = Map.fromList
  [ ("encodeTx",       (encodeTx, "Encode a transaction to rlp"))
  ]


runJsonRpc :: Value -> ExceptT Err IO Value
runJsonRpc val = do
  let res = parseEither parseRequest val
  (name, params) <- ExceptT $ pure $ over _Left invalidRequest res
  runMethod name params
  where
    invalidRequest = errStr InvalidProtocol
    parseRequest = withObject "request" $ \obj ->
      (,) <$> obj .: "method" <*> obj .: "params"


runMethod :: String -> Value -> ExceptT Err IO Value
runMethod name params = do
  let throw = throwE $ errStr InvalidMethod name
  (method,_) <- maybe throw pure $ Map.lookup name methods
  method params


wrapJson :: Either Err Value -> Value
wrapJson = either wrapJsonError wrapSuccess
  where
    wrapSuccess val = object ["result" .= val]
    wrapJsonError val = object ["error" .= val]


-- secp256k1KeyPair :: JsonMethod
-- secp256k1KeyPair _ = do
--   sk <- lift $ withSource getEntropy genPrvKey
--   let wif = decodeUtf8 $ toWif sk
--       pk = derivePubKey sk
--   return $ object [ "pubKey" .= pk
--                   , "wif" .= wif
--                   , "addr" .= pubKeyAddr pk
--                   ]


showErrorClasses :: JsonMethod
showErrorClasses _ = return $ object ["errors" .= allErrorClasses]
