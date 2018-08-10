{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Base16 as B16

import           Network.Ethereum.Crypto
import           Network.Ethereum.Transaction
import           Network.Hath.Data.Aeson
import           Network.Hath.Prelude

import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit

import           TestBitcoinContracts
import           TestHashTrie
import Debug.Trace


main :: IO ()
main = defaultMain $ testGroup "All tests"
  [ recSigTests
  , contractTests
  , hashTrieTests
  ]


recSigTests :: TestTree
recSigTests = testGroup "RecSig tests"

  [ testCase "CompactRecSig" $ do
      let Just recSig = signRecMsg sk <$> msg (unSha3 txid_a)
          CompactRecSig r s v = exportCompactRecSig recSig
          d = B16.encode . fromShort
      (d r,d s,v) @?= txsig_a

  , testCase "Get txid" $ do
      txid tx_a @?= txid_a

  , testCase "Encode tx" $ do
      encodeTx tx_a @?= txbin_a

  , testCase "Recover" $ do
      let signed = signTx tx_a sk
          Just pk = recoverFrom signed
      pubKeyAddr pk @?= address

  , testCase "encodeSpecialV" $ do

      encodeSpecialV 0 16 @?= 67

      forM_ [0..512] $ \i -> do
        decodeSpecialV (encodeSpecialV 0 i) @?= (0,i)
        decodeSpecialV (encodeSpecialV 1 i) @?= (1,i)

  ]

  
sk :: SecKey
(Just sk) = secKey "11111111111111111111111111111111"

address :: Address
address = Address $ fst $ B16.decode "77952ce83ca3cad9f7adcfabeda85bd2f1f52008"

txid_a :: Sha3
txid_a = "d018f1502a71f61a00b77546b99f2a647dda07ecb4cf94bd14cd4dbf4337be3d"

txbin_a :: ByteString
txbin_a = fst $ B16.decode "ce800a82cfc6808204d281f4108080"

tx_a :: Transaction
tx_a = Tx { _nonce = 0
          , _value = 1234
          , _to = Nothing
          , _sig = Nothing
          , _gasPrice = 10
          , _gas = 53190
          , _data = "\xf4"
          , _chainId = 16
          }

txsig_a :: (ByteString, ByteString, Word8)
txsig_a = ( "14f469b1b8022b411cbe6e6b19b21578223bb81be0f32048bc258baa905a47d0"
          , "37a2c7cc63e8f8b3523700ad23709e2ae7582e62b78b7e6e178c203b5a863e68"
          , 1
          )
