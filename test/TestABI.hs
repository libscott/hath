{-# LANGUAGE OverloadedStrings #-}

module TestABI where

import qualified Data.ByteString as BS
import           Data.Either
import           Network.Ethereum.Data

import           Hath.Prelude

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.QuickCheck.Instances
import           Test.Tasty.QuickCheck hiding (Fixed)


abiTests, solidityExamples, parity3 :: TestTree
abiTests = testGroup "abi" [ solidityExamples, parity3, misc ]


-- https://solidity.readthedocs.io/en/develop/abi-spec.html
solidityExamples = testGroup "solidity examples"
  [ testCase "example1" $
      
      iso (69::Int, True)
          ("0000000000000000000000000000000000000000000000000000000000000045" <>
           "0000000000000000000000000000000000000000000000000000000000000001"
          )

  , testCase "example2" $

      iso (BytesN "abc", BytesN "def")
          ("6162630000000000000000000000000000000000000000000000000000000000" <>
           "6465660000000000000000000000000000000000000000000000000000000000"
          )

  , testCase "example3" $
    
      iso (("dave", True, [1,2,3]) :: (ByteString, Bool, [Int]))
          ("0000000000000000000000000000000000000000000000000000000000000060" <>
           "0000000000000000000000000000000000000000000000000000000000000001" <>
           "00000000000000000000000000000000000000000000000000000000000000a0" <>
           "0000000000000000000000000000000000000000000000000000000000000004" <>
           "6461766500000000000000000000000000000000000000000000000000000000" <>
           "0000000000000000000000000000000000000000000000000000000000000003" <>
           "0000000000000000000000000000000000000000000000000000000000000001" <>
           "0000000000000000000000000000000000000000000000000000000000000002" <>
           "0000000000000000000000000000000000000000000000000000000000000003"
          )
  ]


-- https://github.com/paritytech/js-abi/issues/3
parity3 = testCase "parity3" $

      iso (([[]], [[]]) :: ([[U256]],[[U256]]))
          ("0000000000000000000000000000000000000000000000000000000000000040" <>
           "00000000000000000000000000000000000000000000000000000000000000a0" <>
           "0000000000000000000000000000000000000000000000000000000000000001" <>
           "0000000000000000000000000000000000000000000000000000000000000080" <>
           "0000000000000000000000000000000000000000000000000000000000000000" <>
           "0000000000000000000000000000000000000000000000000000000000000001" <>
           "00000000000000000000000000000000000000000000000000000000000000e0" <>
           "0000000000000000000000000000000000000000000000000000000000000000"
          )


misc = testGroup "misc"
  [ testCase "misc1" $ abiPass ()
  , testCase "misc2" $ abiPass ((), (), ())
  , testCase "misc3" $ abiPass (U256 1, U256 2)
  , testCase "misc4" $ abiPass [("ABC"::ByteString,True)]
  , testCase "misc5" $ abiPass [("ABC"::ByteString,True),("DEF",False)]
  , testCase "misc6" $ abiPass ([["ABC"::ByteString,"DEF"],["GHI"]], [(BytesN "a", "def"::ByteString)])
  , testCase "misc7" $ abiPass ("abc"::ByteString,("def"::ByteString,True))
  , testCase "misc8" $ abiPass ([([(U256 1,"abc"::ByteString)],True)],0::Int)
  , testCase "misc9" $ abiPass (BytesN "")
  , testCase "fail1" $ abiFail (BytesN "1") (undefined::Int)
  , testCase "fail2" $ isLeft (decodeABI "" :: Either String Int) @? "string is not long enough"
  ]


iso :: (PutABI a, GetABI a, Eq a, Show a) => a -> ByteString -> IO ()
iso from to = do 
  bsWords (toHex $ encodeABI from) @?= bsWords to
  decodeABI (fromHex to) @?= Right from

abiPass :: (PutABI a, GetABI a, Eq a, Show a) => a -> IO ()
abiPass a = decodeABI (encodeABI a) @?= Right a

abiFail :: (PutABI a, GetABI t) => a -> t -> IO ()
abiFail a t = isLeft (f a t) @? "should fail"
  where
    f :: (PutABI a, GetABI t) => a -> t -> Either String t
    f a _ = decodeABI (encodeABI a)

bsWords :: ByteString -> [ByteString]
bsWords "" = []
bsWords bs = let (a,b) = BS.splitAt 64 bs in a : bsWords b
