{-# LANGUAGE OverloadedStrings #-}

module Network.JsonRpc where

import           Data.Conduit hiding (connect)
import           Data.Conduit.JSON.NewlineDelimited
import qualified Data.Conduit.List as CL
import           Data.Conduit.Network

import           Network.HTTP.Simple
import           Network.Socket hiding (send, recv)
import           Network.Socket.ByteString

import           Hath.Data.Aeson
import           Hath.Monad
import           Hath.Prelude


runJsonRpc :: FromJSON a => Text -> Value -> (Value -> Hath r Value) -> Hath r a
runJsonRpc method params act = do
  let body = "{jsonrpc,method,params,id}" .% (String "2.0", method, params, Null)
      interpret v = case (v .? "{error:{message}}", v .? "{result}") of
                      (Nothing, Just r) -> pure r
                      (Just e, _)       -> error e
                      _                 -> error $ "Unexpected response" ++ show v
  act body >>= interpret

queryHttp :: String -> Value -> Hath r Value
queryHttp endpoint body = do
  let req = setRequestBodyJSON body $ fromString $ "POST " ++ endpoint
  response <- httpJSONEither req
  case getResponseBody response of
       Left e -> error $ show e
       Right out -> pure out

queryIpc :: FilePath -> Value -> Hath r Value
queryIpc endpoint body = do
  out <- liftIO $ do
    sock <- socket AF_UNIX Stream 0
    connect sock $ SockAddrUnix endpoint
    let mResponse = maybe (Left "No response") id <$> await
        conduit = do
          yield body .| serializer .| sinkSocket sock
          sourceSocket sock .| eitherParser .| mResponse
    runConduit conduit <* close sock
  either error pure out

queryJsonRpc :: (FromJSON a, ToJSON p) => String -> Text -> p -> Hath r a
queryJsonRpc endpoint method params =
  traceE ("Json RPC: " ++ show (endpoint, method, asString $ toJSON params)) $ do
    let transport = if take 4 endpoint == "http" then queryHttp else queryIpc
    runJsonRpc method (toJSON params) $ transport endpoint
