{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

module Hath.Mandate.Types where

import           Network.Ethereum.Crypto
import           Network.Ethereum.Data
import           Network.Ethereum.RPC

import           Hath.Prelude
import           Hath.Mandate.Round


data Mandate = Mandate
  { getAddress :: Address
  , getMe :: Ident
  , getChainId :: Integer
  , getAppKey :: Bytes 32
  , getProc :: AgreementProcess
  }

instance Has AgreementProcess Mandate where
  has = getProc


mandateGetMembers :: (Has Mandate r, Has GethConfig r) => Hath r (Int, [Address])
mandateGetMembers = do
  addr <- asks $ getAddress . has
  unABI <$> readCall addr (abi "getMembers()" ())
