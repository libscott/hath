{-# LANGUAGE DataKinds, OverloadedStrings #-}

module TestRound where

import           Data.Bits
import qualified Data.Map as Map

import           Network.Ethereum.Crypto
import           Hath.Consensus.Round
import           Hath.Prelude

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.QuickCheck.Instances
import           Test.Tasty.QuickCheck hiding (Fixed)


members :: [Address]
members = 
  [ "0x90f8bf6a479f320ead074411a4b0e7944ea8c9c1"
  , "0xffcf8fdee72ac11b5c542428b35eef5769c409f0"
  , "0x22d491bde2303f2f43325b2108d26f1eaba1e32b"
  , "0xe11ba2b4d45eaed5996cd0823791e0c93114882d"
  , "0xd03ea8624c8c5987235048901fb614fdca89b117"
  , "0x95ced938f7991cd0dfcb48f0a06a40fa1af46ebc"
  , "0x3e5e9111ae8eb78fe1cc3bb8915d5d461f3ef9a9"
  , "0x28a8746e75304c0780e011bed21c72cd78cd535e"
  , "0xaca94ef8bd5ffee41947b4585a84bda5a3d3da6e"
  , "0x1df62f291b2e969fb0849d99d9ce41e2f137006e"
  ]


exampleMap :: [Int] -> Map Address ()
exampleMap idxs = Map.fromList [(members!!i, ()) | i <- idxs]

mkIdx :: [Int] -> Integer
mkIdx = foldl setBit 0

roundPureTests :: TestTree
roundPureTests = testGroup "round pure tests"
  [ testCase "inventoryIndex" $ do
      inventoryIndex members (exampleMap [1,3,5]) @?= 42

  , testCase "prioritiseRemoteInventory" $ do
      let myInv = exampleMap [0,1,2]
      let remotes =
            [ ('a', mkIdx [1,2,3])
            , ('b', mkIdx [2,3,4])
            , ('c', mkIdx [3,4,5])
            , ('d', mkIdx [1])
            ]
      prioritiseRemoteInventory members myInv remotes
        @?= [ ('c', mkIdx [3,4,5])
            , ('b', mkIdx [3,4])
            , ('a', mkIdx [3])
            , ('d', mkIdx [])
            ]

  , testCase "dedupeInventoryQueries" $ do
      let remotes =
            [ ('a', mkIdx [1,2,3])
            , ('b', mkIdx [2,3,4])
            , ('c', mkIdx [3,4,5])
            , ('d', mkIdx [1])
            ]
      dedupeInventoryQueries remotes
        @?= [ ('a', mkIdx [1,2,3])
            , ('b', mkIdx [4])
            , ('c', mkIdx [5])
            ]
  ]
