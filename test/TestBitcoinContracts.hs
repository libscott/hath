{-# LANGUAGE OverloadedStrings #-}

module TestBitcoinContracts where

import           Data.Serialize

import           Network.Haskoin.Crypto
import           Network.Haskoin.Script
import           Network.Haskoin.Script.Evaluator
import           Network.Haskoin.Test

import           Network.Hath.Prelude
import           Network.Hath.Contracts.Bitcoin

import           Test.QuickCheck

import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit


arbitraryBitcoinMultisig :: Gen (Script, Int, [PubKey], [PrvKey])
arbitraryBitcoinMultisig = do
  n <- choose (1,6)
  m <- choose (n,6)
  (sks,pks) <- unzip <$> vectorOf m arbitraryPubKey
  pure (largeBitcoinMultiSig n pks, n, pks, sks)


contractTests :: TestTree
contractTests = testGroup ""
  [
    testCase "largeBitcoinMultisig" $ do
      (redeemScript, n, pks, sks) <- generate arbitraryLargeBitcoinMultisig
      let scriptOutput = PayScriptHash $ addressHash $ encode redeemScript
          scriptPubKey = encodeOutput scriptOutput
          msg = hash256 ("" :: ByteString)
          sigs = signMsg msg <$> sks
          sigOps = [opPushData (encode sig) | sig <- reverse sigs] ++ [opPushData $ encode redeemScript]
          scriptSig = Script sigOps

      let sigCheck ops sig pk = True
          r = execScript scriptSig scriptPubKey sigCheck [read "P2SH"]

      case checkStack . runStack <$> r of
           Left e -> fail (show e)
           Right o -> assert o

      print scriptOutput
      pure ()

  ]
