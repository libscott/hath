module Network.Ethereum.Transaction
  ( module ALL
  , encodeTx
  , signTx
  , recoverFrom
  , txid
  , withoutSig
  ) where

import Network.Ethereum.Crypto
import Network.Ethereum.Prelude
import Network.Ethereum.Transaction.Types as ALL


withoutSig :: Transaction -> Transaction
withoutSig tx = tx { _from = Nobody }

encodeTx :: Transaction -> ByteString
encodeTx = rlpSerialize . rlpEncode

txid :: Transaction -> ByteString
txid = sha3 . encodeTx . withoutSig

signTx :: Transaction -> SecKey -> Transaction
signTx tx sk = 
  let payload = txid tx
      Just recSig = signRecMsg sk <$> msg payload
      sig = exportCompactRecSig recSig
   in tx { _from = Signed sig }

recoverFrom :: Transaction -> Maybe PubKey
recoverFrom tx@(Tx { _from = (Signed crs) }) = do
  message <- msg $ txid tx
  rs <- importCompactRecSig crs
  recover rs message
recoverFrom _ = Nothing

