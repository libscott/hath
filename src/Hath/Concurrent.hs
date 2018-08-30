
module Hath.Concurrent where


import           Control.Concurrent
import           Control.Exception.Safe as E
import           Control.Monad.IO.Class
import qualified Data.Map as Map
import           Hath.Prelude

-- Parallel computation

parM_ :: Int -> [a] -> (a -> Hath r b) -> Hath r ()
parM_ s i a = parM s i a >> pure ()

parM :: Int -> [a] -> (a -> Hath r b) -> Hath r [b]
parM initialSlots items act = do
  r <- ask
  mvar <- liftIO $ newEmptyMVar
  parent <- liftIO $ myThreadId
  
  let fork (i,o) = do
        let ef = do
              res <- runHath r (act o)
              putMVar mvar (i, res)
        forkIO $ ef `catchAny` E.throwTo parent

  let run slots rmap rest
        | slots == initialSlots && null rest = do
            pure $ snd <$> Map.toAscList rmap
        | slots > 0 && not (null rest) = do
            let (j:xs) = rest
            fork j
            run (slots-1) rmap xs
        | otherwise = do
            (i, res) <- takeMVar mvar
            let nmap = Map.insert i res rmap
            run (slots+1) nmap rest

  liftIO $ run initialSlots Map.empty $ zip [0..] items

-- get some blocks: hath $ parM 10 [100000..] $ \n -> (n,) <$> (length . ethBlockTransactions <$> eth_getBlockByNumber n) >>= logInfo . show

