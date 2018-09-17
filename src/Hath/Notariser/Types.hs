
module Hath.Notariser.Types where

import qualified Data.ByteString.Char8 as C8
import Hath.Prelude
import Network.Ethereum.Crypto
import Network.Ethereum.Data

type Symbol = String

data ProofPacket p = ProofPacket
  { payload :: p
  , reference :: ByteString
  , parts :: [MerkleProof]
  } deriving (Show)

data MerkleProof =
  TrieMerkleProof { index :: RLPObject
                  , trie :: Trie
                  }
  deriving (Show)

instance RLPSerializable p => RLPSerializable (ProofPacket p) where
  rlpDecode = error "rlpDecode for proofs undefined"
  rlpEncode ProofPacket{..} = RLPArray $
    [rlpEncode payload, rlpEncode reference, rlpEncode parts]

instance RLPSerializable MerkleProof where
  rlpDecode = error "rlpDecode for proofs undefined"
  rlpEncode TrieMerkleProof{..} = RLPArray $
    [rlpId 0, index, rlpEncode trie]

rlpId :: Integer -> RLPObject
rlpId = rlpEncode
