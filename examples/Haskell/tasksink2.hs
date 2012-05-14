module Main where

import System.ZMQ
import Control.Monad (forM_)
import System.Random (randomRIO)
import System.IO (hSetBuffering, stdout, BufferMode(..))
import Data.Time.Clock
import Data.ByteString.Char8 (pack)

main :: IO ()
main = withContext 1 $ \context -> do  
  withSocket context Pull $ \receiver -> do
    bind receiver "tcp://*:5558"
    withSocket context Pub $ \controller -> do
        bind controller "tcp://*:5559"
        
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
        let elapsedMsec = elapsedTime * 1000
        putStrLn $ unwords ["Total elapsed time:", show elapsedMsec, "msecs"]

        send controller (pack "KILL") []