{-# LANGUAGE DataKinds, OverloadedStrings #-}

module TestKMDNotarisation where

import qualified Data.ByteString as BS
import           Data.Either
import           Data.Serialize

import           Network.Komodo
import           Network.Ethereum.Crypto

import           Hath.Prelude

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.QuickCheck.Instances
import           Test.Tasty.QuickCheck hiding (Fixed)


kmdNotarisationOpReturnTests = testGroup "notarisation opreturn"
  [ testCase "example1" $ do
      let bs = "7c1f732e179cdbad6e328652b0bc57e5dc7a8c7739f98810830e439bbfca37600600000054455354455448008b319d02cdcb8f4dd699ef3b414d10cf8fe5070e7a0c529ff2b4b653f2b7f637010057fe"
      let nor = NOR "7c1f732e179cdbad6e328652b0bc57e5dc7a8c7739f98810830e439bbfca3760"
                    6 "TESTETH"
                    "8b319d02cdcb8f4dd699ef3b414d10cf8fe5070e7a0c529ff2b4b653f2b7f637"
                    1 65111 :: NotarisationData Sha3
      decode (fromHex bs) @?= Right nor
      toHex (encode nor) @?= bs
  ]
