{-# LANGUAGE DataKinds, OverloadedStrings #-}

module TestKMDNotarisation where

import qualified Data.ByteString as BS
import           Data.Either
import           Data.Serialize

import           Network.Komodo

import           Hath.Prelude

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.QuickCheck.Instances
import           Test.Tasty.QuickCheck hiding (Fixed)


kmdNotarisationOpReturnTests = testGroup "notarisation opreturn"
  [ testCase "example1" $ do
      let bs = "6037cabf9b430e831088f939778c7adce557bcb05286326eaddb9c172e731f7c06000000544553544554480037f6b7f253b6b4f29f520c7a0e07e58fcf104d413bef99d64d8fcbcd029d318b010057fe"
      let nor = NOR "7c1f732e179cdbad6e328652b0bc57e5dc7a8c7739f98810830e439bbfca3760"
                    6 "TESTETH"
                    "8b319d02cdcb8f4dd699ef3b414d10cf8fe5070e7a0c529ff2b4b653f2b7f637"
                    1 65111
      decode (fromHex bs) @?= Right nor
      toHex (encode nor) @?= bs
  ]
