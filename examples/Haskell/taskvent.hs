-- |
-- Task ventilator
-- Binds PUSH socket to tcp://*:5557
-- Sends batch of tasks to workers via that socket
-- 
-- Translated to Haskell by ERDI Gergo http://gergo.erdi.hu/

module Main where

import System.ZMQ3.Monadic(runZMQ, socket, bind, connect, send, Push(..), liftIO)
import Control.Monad (replicateM)
import Data.ByteString.Char8 (pack)
import System.Random (randomRIO)
import Control.Applicative ((<$>))
import System.IO (hFlush, stdout)
import Control.Concurrent (threadDelay)

main :: IO ()
main = 
      runZMQ $ do 
            sender <- socket Push 
            bind sender "tcp://*:5557"

            sink <- socket Push
            connect sink "tcp://localhost:5558"

            liftIO $ putStr "Press Enter when the workers are ready:" >> hFlush stdout
            liftIO $ getLine >> putStrLn "Sending tasks to workers..."

                 --  The first message is "0" and signals start of batch
            send sink [] (pack "0")

            -- Send 100 tasks, calculate total workload
            workload <- sum <$> replicateM 100 (do
                workload' <- liftIO $ (randomRIO (1, 100) :: IO Int)
                send sender [] (pack $ show workload') 
                return workload')
              
            liftIO $ putStrLn $ unwords ["Total expected cost:", show workload, "msecs"]
            -- Give 0MQ time to deliver
            liftIO $ threadDelay $ 1 * 1000 * 1000
