{-# LANGUAGE OverloadedStrings #-}

module TestTrie where

import           Data.List (nub)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString as BS

import           Network.Ethereum.Data
import           Network.Ethereum.Crypto
import           Network.Ethereum.Crypto.Trie
import           Hath.Prelude

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

trieTests :: TestTree
trieTests = testGroup "trie"
  [ testProperty "proof checking" $ forAll arbitraryProofTrie prop_proof

  , testCase "regression 1" $
      let trie = Branch16 [N,N,N,Branch16 [N,Leaf [15,7] " ",N,N,N,Branch16 [N,N,N,N,N,N,Branch16 [N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N] "",N,N,N,N,N,N,N,N,N] "",N,N,N,N,N,N,N,N,N,N] " ",N,N,N,N,N,N,N,N,N,N,N,N] " "
       in prop_proof (trie, [3,1,15,7], " ") @?= True

  , testCase "cpp-ethereum test" $
      let trie = mapToTrie [("a","A"),("b","B")]
       in trieHex trie @?= "d716d580c22041c220428080808080808080808080808080"

  , testCase "branch16" $
      let trie = hexMapToTrie [([], "a"), ([1], "b")]
       in trie @?= Branch16 (N : Leaf [] "b" : replicate 14 N) "a"

  , testCase "empty trie" $
      mapToTrie [] @?= N

  , testCase "orderedTrie0" $ do
      let trie = orderedTrie ["\1", "\2"]
      trieHex trie @?= "d5c2310280808080808080c230018080808080808080"

  , testCase "orderedTrie1" $ do
      -- the trieProof call is a hacky way to get it to hash the other nodes
      let trie = trieProof [] "" $ orderedTrie ["\2", BS.replicate 33 1]
      trieHex trie @?= "f3a0c39fbab0f2858a1c2bda8dadedad028c4ed691f23955af87777241a4dad9e2f380808080808080c230028080808080808080"
      trieRoot trie @?= "68e16fd2af096a850edfc71879cf931ab59e05b2688d450270935020f92387e8"
  ]

trieHex :: Trie -> ByteString
trieHex = toHex . rlpSerialize . rlpEncode


hexPrefixTests :: TestTree
hexPrefixTests = testGroup "hexPrefix"
  [ testCase "go1" $ do
      hexPrefixDecode "\x12\x34\x56" @?= (False, [2, 3, 4, 5, 6])

  , testCase "go2" $ do
      hexPrefixDecode "\x12\x34\x5" @?= (False, [2, 3, 4, 0, 5])

  , testCase "go3" $ do
      hexPrefixDecode "\x20\x0f\x1c\xb8" @?= (True, [0, 15, 1, 12, 11, 8])

  , testCase "go4" $ do
      hexPrefixEncode [15, 1, 12, 11, 8] True @?= (BS.pack [0x3f, 0x1c, 0xb8])

  , testCase "go5" $ do
      testIso ([], False) (BS.pack [0x00])

  , testCase "go6" $ do
      testIso ([], True) (BS.pack [0x20])
  
  , testCase "go7" $ do
      testIso ([1, 2, 3, 4, 5], False) (BS.pack [0x11, 0x23, 0x45])

  , testCase "go8" $ do
      testIso ([0, 1, 2, 3, 4, 5], False) (BS.pack [0x00, 0x01, 0x23, 0x45])


  , testCase "cpp01" $ do
      testIso ([0,0,1,2,3,4,5], False) $ fromHex "0x10012345"
  , testCase "cpp02" $ do
      testIso ([0,1,2,3,4,5], False)   $ fromHex "0x00012345"
  , testCase "cpp03" $ do
      testIso ([1,2,3,4,5], False)     $ fromHex "0x112345"
  , testCase "cpp04" $ do
      testIso ([0,0,1,2,3,4], False)   $ fromHex "0x00001234"
  , testCase "cpp05" $ do
      testIso ([0,1,2,3,4], False)     $ fromHex "0x101234"
  , testCase "cpp06" $ do
      testIso ([1,2,3,4], False)       $ fromHex "0x001234"
  , testCase "cpp07" $ do
      testIso ([0,0,1,2,3,4,5], True)  $ fromHex "0x30012345"
  , testCase "cpp08" $ do
      testIso ([0,0,1,2,3,4], True)    $ fromHex "0x20001234"
  , testCase "cpp09" $ do
      testIso ([0,1,2,3,4,5], True)    $ fromHex "0x20012345"
  , testCase "cpp10" $ do
      testIso ([1,2,3,4,5], True)      $ fromHex "0x312345"
  , testCase "cpp11" $ do
      testIso ([1,2,3,4], True)        $ fromHex "0x201234"

  ]

testIso :: (Nibbles, Bool) -> ByteString -> IO ()
testIso (n,t) bs = do
  hexPrefixEncode n t @?= bs
  hexPrefixDecode bs @?= (t, n)
