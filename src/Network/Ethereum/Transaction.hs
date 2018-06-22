module Network.Ethereum.Transaction
  ( module ALL
  , signTx
  , withoutSig
  ) where

import Network.Ethereum.Crypto
import Network.Ethereum.Prelude
import Network.Ethereum.Transaction.Types as ALL


withoutSig :: Transaction -> Transaction
withoutSig tx = tx { _from = Nobody }


signTx :: Transaction -> SecKey -> Transaction
signTx tx sk = 
  let payload = sha3 $ rlpSerialize $ rlpEncode $ withoutSig tx
      Just recSig = signRecMsg sk <$> msg payload
      sig = exportCompactRecSig recSig
   in tx { _from = Signed sig }
