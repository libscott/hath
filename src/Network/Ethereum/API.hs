{-# LANGUAGE OverloadedStrings #-}

module Network.Ethereum.API
  ( module API
  , wrapJson
  ) where

import           Data.Aeson.Types
import qualified Data.Map as Map

import           Lens.Micro

import           Network.Ethereum.API.Utils as API
import           Network.Ethereum.Crypto
import           Network.Hath.Prelude


wrapJson :: Either Err Value -> Value
wrapJson = either wrapJsonError wrapSuccess
  where
    wrapSuccess val = object ["result" .= val]
    wrapJsonError val = object ["error" .= val]


secp256k1KeyPair :: JsonMethod
secp256k1KeyPair _ = do
  sk <- lift genSecKey
  let pk = derivePubKey sk
  return $ object [ "pubKey" .= (show pk)
                  , "sk" .= show sk
                  , "address" .= show (pubKeyAddr pk)
                  ]


showErrorClasses :: JsonMethod
showErrorClasses _ = return $ object ["errors" .= allErrorClasses]
