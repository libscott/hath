
module Hath.Consensus.Utils where

import Control.Distributed.Process
import Hath.Prelude
import Hath.Prelude.Lifted


repeatMatch :: Int -> [Match ()] -> Process ()
repeatMatch timeout matches = do
  startTime <- getCurrentTime
  fix $ \f -> do
    d <- timeDelta startTime
    let us = timeout - d
    when (us > 0) $
       receiveTimeout us matches >>= maybe (pure ()) (const f)
