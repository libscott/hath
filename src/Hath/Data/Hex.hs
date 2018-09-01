{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hath.Data.Hex
  ( Hex(..)
  , U256(..)
  ) where

import qualified Data.Text as T

import           Data.Char
import           Hath.Data.Aeson
import           Hath.Prelude


newtype Hex = Hex { unHex :: ByteString }

instance Show Hex where
  show (Hex bs) = show $ "0x" <> toHex bs

instance Read Hex where
  readsPrec a s
    | take 2 s == "0x" = readsPrec a $ drop 2 s
    | otherwise        = [(Hex $ fromHex $ fromString s, "")]

instance ToJSON Hex where
  toJSON (Hex bs) = String $ decodeUtf8 $ "0x" <> toHex bs
  {-# INLINABLE toJSON #-}

instance FromJSON Hex where
  parseJSON val = do
    s <- parseJSON val
    let r = if T.take 2 s == "0x" then T.drop 2 s else s
     in Hex <$> fromJsonHex (String r)
  {-# INLINABLE parseJSON #-}


-- U256 -----------------------------------------------------------------------

newtype U256 = U256 { unU256 :: Integer }
  deriving (Eq, Ord, Enum, Num, Integral, Real)

instance FromJSON U256 where
  parseJSON v = do
    let toDec c = maybe (fail "Invalid hex char") pure $
          lookup c $ zip "0123456789abcdef" [0..]
        un a b = a * 16 + fromIntegral b
    (pre, body) <- splitAt 2 <$> parseJSON v
    r <- foldl un 0 <$> mapM toDec body
    if pre == "0x" then pure $ U256 r else fail "Invalid hex prefix"
  {-# INLINABLE parseJSON #-}

instance ToJSON U256 where
  toJSON (U256 n) =
    let hexChar i = chr $ i + if i < 10 then 48 else 87
        hex 0 s = "0x" <> s
        hex i s = hex d $ c : s
          where (d,q) = divMod i 16
                c = hexChar $ fromIntegral q
     in toJSON $ hex n $ if n == 0 then "0" else ""
  {-# INLINABLE toJSON #-}

instance Show U256 where
  show (U256 i) = show i
