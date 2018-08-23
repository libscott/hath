{-# LANGUAGE DataKinds #-}

module Hath.Mandate.Types where

import           Network.Ethereum.Crypto
import           Network.Ethereum.Data
import           Network.Ethereum.RPC

import           Hath.Prelude


data Mandate = Mandate
  { getAddress :: Address
  , getMe :: Ident
  , getChainId :: Integer
  , getAppKey :: Bytes 32
  }


mandateGetMembers :: (Has Mandate r, Has GethConfig r) => Hath r (Int, [Address])
mandateGetMembers = do
  addr <- asks $ getAddress . has
  unABI <$> readCall addr (abi "getMembers()" ())
