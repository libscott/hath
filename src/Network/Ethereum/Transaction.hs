module Network.Ethereum.Transaction
  ( module ALL
  , encodeTx
  , signTx
  , recoverFrom
  , txid
  , withoutSig
  ) where

import Network.Ethereum.Crypto
import Network.Ethereum.Data.RLP
import Network.Ethereum.Transaction.Types as ALL

import Network.Hath.Prelude


withoutSig :: Transaction -> Transaction
withoutSig tx = tx { _sig = Nothing }

encodeTx :: Transaction -> ByteString
encodeTx = rlpSerialize . rlpEncode

txid :: Transaction -> Sha3
txid = sha3 . encodeTx . withoutSig

signTx :: Transaction -> SecKey -> Transaction
signTx tx sk = 
  let payload = unSha3 $ txid tx
      Just recSig = signRecMsg sk <$> msg payload
      sig = exportCompactRecSig recSig
   in tx { _sig = Just sig }

recoverFrom :: Transaction -> Maybe PubKey
recoverFrom tx = do
  rs <- _sig tx >>= importCompactRecSig
  message <- msg $ unSha3 $ txid tx
  recover rs message

