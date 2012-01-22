-- |
-- Task ventilator
-- Binds PUSH socket to tcp://*:5557
-- Sends batch of tasks to workers via that socket
-- 
-- Translated to Haskell by ERDI Gergo http://gergo.erdi.hu/

module Main where

import System.ZMQ
import Control.Monad (replicateM)
import Data.ByteString.Char8 (pack)
import System.Random (randomRIO)
import Control.Applicative ((<$>))
import System.IO (hFlush, stdout)
import Control.Concurrent (threadDelay)

main :: IO ()
main = withContext 1 $ \context -> do  
  withSocket context Push $ \sender -> do
    bind sender "tcp://*:5557"
    withSocket context Push $ \sink -> do
      connect sink "tcp://localhost:5558"
    
      putStr "Press Enter when the workers are ready:"
      hFlush stdout
      _ <- getLine
      putStrLn "Sending tasks to workers..."
  
      --  The first message is "0" and signals start of batch
      send sink (pack "0") []

      -- Send 100 tasks, calculate total workload
      workload <- sum <$> replicateM 100 (do
          workload' <- randomRIO (1, 100) :: IO Int
          send sender (pack $ show workload') []
          return workload')
      
      putStrLn $ unwords ["Total expected cost:", show workload, "msecs"]
      -- Give 0MQ time to deliver
      threadDelay $ 1 * 1000 * 1000
