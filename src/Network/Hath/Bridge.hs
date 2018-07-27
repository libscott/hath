{-# LANGUAGE OverloadedStrings #-}

module Network.Hath.Bridge where

import           Data.Conduit hiding (connect)
import           Data.Conduit.JSON.NewlineDelimited
import qualified Data.Conduit.List as CL
import           Data.Conduit.Network

import           Network.Socket hiding (send, recv)
import           Network.Socket.ByteString

import           Network.Hath.Data.Aeson
import           Network.Hath.Prelude



queryJsonRPC :: FromJSON a => Socket -> Text -> [Value] -> IO (Either String a)
queryJsonRPC sock method params = do
  let req = "{jsonrpc,method,params,id}" .% (String "2.0", method, params, Null)
      interpret v = case (v .? "{error:{message}}", v .?"{result}") of
                      (Nothing, Just r) -> Right r
                      (Just e, _)       -> Left e
                      _                 -> Left ("Unexpected response" ++ show v)
      mResponse = maybe (Left "No response") id <$> await
  out <- runConduit $ do
      yield req .| serializer .| sinkSocket sock
      sourceSocket sock .| eitherParser .| mResponse
  pure $ out >>= interpret


ethereumRPCSock :: IO Socket
ethereumRPCSock = do
  sock <- socket AF_UNIX Stream 0
  connect sock $ SockAddrUnix "/home/scott/.kmdx/geth.ipc"
  pure sock


queryEthereum :: FromJSON a => Text -> [Value] -> IO (Either String a)
queryEthereum method params = do
  sock <- ethereumRPCSock
  queryJsonRPC sock method params



bridge :: IO ()
bridge = do
  pure ()

