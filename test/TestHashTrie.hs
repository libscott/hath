{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module TestHashTrie where

import           Data.List (nub)
import qualified Data.ByteString.Char8 as BS8

import           Network.Ethereum.Data
import           Network.Ethereum.Crypto
import           Network.Ethereum.Crypto.TrieHash
import           Network.Hath.Prelude

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.QuickCheck.Instances
import           Test.Tasty.QuickCheck
import Debug.Trace


arbitraryProofTrie :: Gen (Trie, Nibbles, ByteString)
arbitraryProofTrie = do
  keys <- listOf1 $ resize 8 $ listOf $ choose (0,15)  :: Gen [Nibbles]
  hexMap <- zip (sort $ nub $ keys) <$> infiniteListOf arbitrary
  let trie = hexMapToTrie hexMap
  (nibs, bs) <- oneof $ pure <$> hexMap
  pure $ (trie, nibs, bs)

prop_proof :: (Trie, Nibbles, ByteString) -> Bool
prop_proof (trie,nibs,bs) =
  trieRoot trie == trieRoot (trieProof nibs bs $ trieProof nibs "" trie)

hashTrieTests :: TestTree
hashTrieTests = testGroup "HashTrie"
  [ testProperty "proof checking" $ forAll arbitraryProofTrie prop_proof

  , testCase "regression 1" $
      let trie = Branch16 [N,N,N,Branch16 [N,Leaf [15,7] " ",N,N,N,Branch16 [N,N,N,N,N,N,Branch16 [N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N] "",N,N,N,N,N,N,N,N,N] "",N,N,N,N,N,N,N,N,N,N] " ",N,N,N,N,N,N,N,N,N,N,N,N] " "
       in prop_proof (trie, [3,1,15,7], " ") @?= True

  , testCase "cpp-ethereum test" $
      let trie = mapToTrie [("a","A"),("b","B")]
       in toHex (rlpSerialize $ rlpEncode trie) @?= "d716d580c22041c220428080808080808080808080808080"

  , testCase "branch16" $
      let trie = hexMapToTrie [([], "a"), ([1], "b")]
       in trie @?= Branch16 (N : Leaf [] "b" : replicate 14 N) "a"

  , testCase "empty trie" $
      mapToTrie [] @?= N
  ]
