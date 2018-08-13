{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Network.Ethereum.RPC where

import           Data.Conduit hiding (connect)
import           Data.Conduit.JSON.NewlineDelimited
import qualified Data.Conduit.List as CL
import           Data.Conduit.Network

import           Network.Socket hiding (send, recv)
import           Network.Socket.ByteString

import           Hath.Data.Aeson
import           Hath.Monad
import           Hath.Prelude


data RPCMaybe a = RPCMaybe (Maybe a)
  deriving (Show)

instance FromJSON a => FromJSON (RPCMaybe a) where
  parseJSON (String "0x") = pure $ RPCMaybe Nothing
  parseJSON val = RPCMaybe . Just <$> parseJSON val


newtype GethConfig = GethConfig { gethIpcPath :: FilePath }
  deriving (Show)


queryJsonRPC :: FromJSON a => Socket -> Text -> [Value] -> HathE r a
queryJsonRPC sock method params = do
  let req = "{jsonrpc,method,params,id}" .% (String "2.0", method, params, Null)
      interpret v = case (v .? "{error:{message}}", v .? "{result}") of
                      (Nothing, Just r) -> Right r
                      (Just e, _)       -> Left e
                      _                 -> Left ("Unexpected response" ++ show v)
      mResponse = maybe (Left "No response") id <$> await
  out <- liftIO $ runConduit $ do
      yield req .| serializer .| sinkSocket sock
      sourceSocket sock .| eitherParser .| mResponse
  traceE ("Ethereum RPC: " ++ asString req) $
    liftEither $ out >>= interpret


ethereumRPCSock :: Hath GethConfig Socket
ethereumRPCSock = do
  ipcPath <- asks $ gethIpcPath
  sock <- liftIO $ socket AF_UNIX Stream 0
  liftIO $ connect sock $ SockAddrUnix ipcPath
  pure sock


queryEthereum :: (Has GethConfig r, FromJSON a) => Text -> [Value] -> HathE r a
queryEthereum method params =
  hasReader $ do
    sock <- lift ethereumRPCSock
    queryJsonRPC sock method params
