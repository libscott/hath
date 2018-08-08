{-# LANGUAGE OverloadedStrings #-}

module Network.Hath.Logging 
  ( module LOG
  , logDebug
  , logInfo
  , logError
  , AsString
  , asString
  ) where

import Data.Aeson
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.Lazy (toStrict)
import Control.Monad.Logger as LOG hiding (logDebug, logInfo, logError)

import Data.String (fromString)

logDebug :: MonadLogger m => String -> m ()
logDebug = logDebugN . fromString

logInfo :: MonadLogger m => String -> m ()
logInfo = logInfoN . fromString

logError :: MonadLogger m => String -> m ()
logError = logErrorN . fromString



class AsString a where
  asString :: a -> String

instance AsString BS8.ByteString where
  asString = BS8.unpack

instance AsString Value where
  asString = asString . toStrict . encode
