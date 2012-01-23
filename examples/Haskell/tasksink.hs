-- |
-- Task sink
-- Binds PULL socket to tcp://localhost:5558
-- Collects results from workers via that socket
-- 
-- Translated to Haskell by ERDI Gergo http://gergo.erdi.hu/

module Main where

import System.ZMQ
import Control.Monad (forM_)
import System.Random (randomRIO)
import System.IO (hSetBuffering, stdout, BufferMode(..))
import Data.Time.Clock (getCurrentTime, diffUTCTime)

main :: IO ()
main = withContext 1 $ \context -> do  
  withSocket context Pull $ \receiver -> do
    bind receiver "tcp://*:5558"
    -- Wait for start of batch
    _ <- receive receiver []
    
    -- Start our clock now
    startTime <- getCurrentTime
                    
    -- Process 100 confirmations
    hSetBuffering stdout NoBuffering
    forM_ [1..100] $ \i -> do
      _ <- receive receiver []
      putStr $ if i `mod` 10 == 0 then ":" else "."
      
    endTime <- getCurrentTime
    let elapsedTime = diffUTCTime endTime startTime
        elapsedMsec = elapsedTime * 1000
    putStrLn $ unwords ["Total elapsed time:", show elapsedMsec, "msecs"]
