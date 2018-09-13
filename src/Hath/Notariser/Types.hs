
module Hath.Notariser.Types where

import qualified Data.ByteString.Char8 as C8
import Hath.Prelude
import Network.Ethereum.Crypto
import Network.Ethereum.Data

type Symbol = String

data ProofPacket = ProofPacket
  { payload :: ByteString
  , reference :: ByteString
  , parts :: [MerkleProof]
  } deriving (Show)

data MerkleProof =
  TrieMerkleProof { index :: ProofIndex
                  , trie :: Trie
                  }
  deriving (Show)

data ProofIndex =
    BlockPosition Integer
  | CrossChain Symbol Integer
  deriving (Show)

instance RLPSerializable ProofPacket where
  rlpDecode = error "rlpDecode for proofs undefined"
  rlpEncode ProofPacket{..} = RLPArray $
    [rlpEncode payload, rlpEncode reference, rlpEncode parts]

instance RLPSerializable MerkleProof where
  rlpDecode = error "rlpDecode for proofs undefined"
  rlpEncode TrieMerkleProof{..} = RLPArray $
    [rlpId 0, rlpEncode index, rlpEncode trie]

instance RLPSerializable ProofIndex where
  rlpDecode = error "rlpDecode for proofs undefined"
  rlpEncode (BlockPosition pos) = RLPArray $
    [rlpId 0, rlpEncode pos]
  rlpEncode (CrossChain symbol pos) = RLPArray $
    [rlpId 1, rlpEncode $ C8.pack symbol, rlpEncode pos]

rlpId :: Integer -> RLPObject
rlpId = rlpEncode
