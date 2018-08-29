
module Hath.Notariser.UTXOs where

import Control.Concurrent (forkIO)
import Control.Exception.Safe (catchAny)
import Control.Monad.Reader
import Data.Scientific

import Network.Bitcoin
import qualified Network.Haskoin.Internals as H

import Hath.Data.Aeson
import Hath.Prelude
import Hath.Prelude.Lifted


monitorUTXOs :: Has BitcoinConfig r
             => Word64 -> Int -> Int -> BitcoinIdent -> Hath r ()
monitorUTXOs amount minimum nsplit (sk, pk, address) = do
  run $ do
    available <- isRightAmount <$> bitcoinUtxos [address]

    when (length available < minimum) $ do
         logInfo $ "Creating UTXOs of " ++ show amount
         makeSplitTx splits >>=
           \case Nothing -> logWarn "Could not create split TX"
                 Just tx -> do
                   splitId <- queryBitcoin "sendrawtransaction" [tx]
                   logInfo $ "Sent split tx: " ++ splitId

         threadDelay $ 5 * 60 * 1000000
    threadDelay $ 30 * 1000000
  where
    isRightAmount = filter ((==amount) . utxoAmount)
    splits = replicate nsplit (H.PayPK pk, amount)
    onError e = do
      runHath () $ logError $ show e
      threadDelay $ 30 * 1000000
    run act = do
      r <- ask
      _ <- liftIO $ forkIO $ forever $ runHath r act `catchAny` onError
      pure ()


makeSplitTx :: Has BitcoinConfig r
            => [(H.ScriptOutput, Word64)] -> Hath r (Maybe H.Tx)
makeSplitTx outs = do
  utxos <- viable <$> bitcoinUtxos []
  case utxos of
       [] -> logWarn "No funds!" >> pure Nothing
       (x:_) -> do
         logInfo $ "Chose input: " ++ show (getOutPoint x)
         case build x of
              Left err -> do
                pure Nothing
              Right tx -> do
                signed <- queryBitcoin "signrawtransaction" [tx]
                pure $ signed .? "{hex}"
  where
    needed = sum $ snd <$> outs
    fee = 10000
    viable = reverse . sortOn (\c -> ( utxoAmount c > 10^9
                                     , utxoConfirmations c * (-1)
                                     , utxoTxid c
                                     ))
                     . filter ((>needed+fee) . utxoAmount)
                     . filter utxoSpendable
    build x =
      let changeSats = utxoAmount x - needed - fee
          changeAddr = H.getAddrHash $ utxoAddress x
          change = if changeSats > fee
                      then [(H.PayPKHash $ changeAddr, changeSats)]
                      else []  -- avoid dust
          input = getOutPoint x
       in H.buildTx [getOutPoint x] (outs++change)
