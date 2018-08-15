{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}

module Network.Ethereum.Data.ABI
  ( ABI(..)
  , PutABI(..)
  , GetABI(..)
  , Bytes
  , bytes
  , abi
  , encodeABI
  , decodeABI
  , takeN
  ) where

import           Control.Monad.Except
import           Control.Monad.State

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Proxy

import           GHC.TypeLits

import           Network.Ethereum.Data.Hex
import           Network.Ethereum.Data.RLP
import           Network.Ethereum.Crypto
import           Hath.Data.Aeson
import           Hath.Prelude



-- Encoding ABI ---------------------------------------------------------------
--

type PutState =
  ( Int           -- ^ Size of fixed section
  , BL.ByteString -- ^ Fixed section
  , BL.ByteString -- ^ Dynamic section
  )

type ABIPutter = State PutState ()

class PutABI a where
  fixedLen :: a -> Int
  fixedLen _ = 32
  putABI :: a -> ABIPutter

abi :: PutABI a => String -> a -> ByteString
abi "" a = encodeABI a
abi method a = toStrict $ abiMethod method <> lazyABI a

encodeABI :: PutABI a => a -> ByteString
encodeABI = toStrict . lazyABI

lazyABI :: PutABI a => a -> BL.ByteString
lazyABI a = runPutABI (fixedLen a) $ putABI a

runPutABI :: Int -> ABIPutter -> BL.ByteString
runPutABI i act = let (_, l, r) = execState act (i, "", "") in l <> r

putData :: BL.ByteString -> ABIPutter
putData bs = modify $ \(a,b,c) -> (a,b<>bs,c)

putDynamic :: Int -> Int -> ABIPutter -> ABIPutter
putDynamic len fixedLen act = do
  off <- gets $ \(i,_,b) -> i + lazyLen b
  putABI off
  let out = runPutABI (off+fixedLen+32) $ putABI len >> act
  modify $ \(a,b,c) -> (a,b,c<>out)

instance PutABI Int where
  putABI i = putABI $ U256 $ fromIntegral i

instance PutABI U256 where
  putABI (U256 i) = putData $ bytesPad (packInteger i) True

instance PutABI ByteString where
  putABI bs =
    let bs' = if bs == "" then "" else bytesPad bs False
     in putDynamic (BS.length bs) 0 $ putData bs'

instance PutABI Address where
  putABI (Address bs) = putData $ bytesPad bs True

instance PutABI () where
  fixedLen () = 0
  putABI () = pure ()

instance (PutABI a, PutABI b) => PutABI (a,b) where
  fixedLen (a,b) = fixedLen a + fixedLen b
  putABI (a,b) = putABI a >> putABI b

instance (PutABI a, PutABI b, PutABI c) => PutABI (a,b,c) where
  fixedLen (a,b,c) = fixedLen a + fixedLen (b,c)
  putABI (a,b,c) = putABI a >> putABI (b,c)

instance (PutABI a, PutABI b, PutABI c, PutABI d) => PutABI (a,b,c,d) where
  fixedLen (a,b,c,d) = fixedLen a + fixedLen (b,c,d)
  putABI (a,b,c,d) = putABI a >> putABI (b,c,d)

instance PutABI a => PutABI [a] where
  putABI xs = do
    let innerLen = sum $ fixedLen <$> xs
    putDynamic (length xs) innerLen $ mapM_ putABI xs

instance forall n. KnownNat n => PutABI (Bytes n) where
  putABI (Bytes bs) =
    if BS.length bs > bytesGetN (Proxy :: Proxy n)
       then error "Bytes: data too long"
       else putData $ bytesPad bs False

instance PutABI Bool where
  putABI = putABI . fromEnum

instance PutABI Value where
  putABI = putABI . BL.toStrict . encode



-- Parsing ABI ----------------------------------------------------------------
--

type GetState =
  ( Int        -- ^ Current position
  , ByteString -- ^ Data being read
  )

type ABIGetter a = ExceptT String (State GetState) a

class GetABI a where
  getABI :: ABIGetter a

decodeABI :: GetABI a => ByteString -> Either String a
decodeABI bs = evalState (runExceptT getABI) (0, bs)

takeN :: Int -> ABIGetter ByteString
takeN n = do
  (off,bs) <- get
  when (n > BS.length bs - off) $ throwError "Not enough input"
  put (off + n, bs)
  pure $ BS.take n $ BS.drop off bs

getDynamic :: GetABI a => ABIGetter a -> ABIGetter a
getDynamic act = do
  st <- (,) <$> getABI <*> gets snd
  either throwError pure $ evalState (runExceptT act) st

instance GetABI Bool where
  getABI = do
    bs <- takeN 32
    case bs of
         "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0" -> pure False
         "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\1" -> pure True
         _ -> throwError $ "Invalid bool: " ++ show bs

instance GetABI Int where
  getABI = do
    U256 uint <- getABI
    when (uint > fromIntegral (maxBound::Int)) $
      throwError $ "Int too large: " ++ show uint
    pure $ fromIntegral uint

instance GetABI U256 where
  getABI = U256 . unpackInteger <$> takeN 32

instance GetABI ByteString where
  getABI =
    getDynamic $ do
      n <- getABI
      let padding = if n == 0 then 0 else roundLen n
      BS.take n <$> takeN (padding + n)

instance forall n. KnownNat n => GetABI (Bytes n) where
  getABI = do
    bs <- takeN 32
    let n = bytesGetN (Proxy :: Proxy n)
    pure $ bytes $ BS.take n bs

instance GetABI Address where
  getABI = Address . BS.drop 12 <$> takeN 32

instance GetABI a => GetABI [a] where
  getABI =
    getDynamic $ do
      n <- getABI
      replicateM n getABI

instance GetABI () where
  getABI = pure ()

instance (GetABI a, GetABI b) => GetABI (a, b) where
  getABI = (,) <$> getABI <*> getABI

instance (GetABI a, GetABI b, GetABI c) => GetABI (a, b, c) where
  getABI = (,,) <$> getABI <*> getABI <*> getABI

instance GetABI Value where
  getABI = do
    bs <- getABI
    if BS.length bs == 0
       then pure Null
       else case decodeStrict' bs of
                    Just r -> pure r
                    Nothing -> throwError "json decode error"

-- Bytes type -----------------------------------------------------------------
--
newtype Bytes (n :: Nat) = Bytes { unBytes :: ByteString }
  deriving (Eq, Ord)

instance Show (Bytes n) where
  show = show . asString . unBytes

instance forall n. KnownNat n => IsString (Bytes n) where
  fromString = bytes . fromString

bytes :: forall n. KnownNat n => ByteString -> Bytes n
bytes =
  let n = bytesGetN (Proxy :: Proxy n)
   in n `seq` Bytes

bytesGetN :: forall n. KnownNat n => Proxy n -> Int
bytesGetN p =
  let n = natVal p
   in if n > 32
         then error $ "n too big: " ++ show n
         else fromIntegral n

-- Aeson instance (for abi inside JSON) ---------------------------------------
--
newtype ABI a = ABI { unABI :: a }
  deriving (Show)

instance GetABI a => FromJSON (ABI a) where
  parseJSON val = do
    Hex bs <- parseJSON val
    either fail pure $ ABI <$> decodeABI bs

-- Utilities ------------------------------------------------------------------
--
abiMethod :: String -> BL.ByteString
abiMethod = BL.fromStrict . BS.take 4 . sha3' . fromString

bytesPad :: ByteString -> Bool -> BL.ByteString
bytesPad bs rev = do
  let len = fromIntegral $ BS.length bs
      padding = BL.replicate (roundLen len) 0
      bl = BL.fromStrict bs
   in if rev then padding <> bl
             else bl <> padding

lazyLen :: BL.ByteString -> Int
lazyLen = fromIntegral . BL.length

roundLen :: Integral a => a -> a
roundLen l = (quot (l - 1) 32 + 1) * 32 - l
