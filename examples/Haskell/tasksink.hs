--  Task sink
--  Binds PULL socket to tcp://localhost:5558
--  Collects results from workers via that socket

module Main where

import Control.Monad
import Data.Time.Clock
import System.IO
import System.ZMQ4.Monadic

main :: IO ()
main = runZMQ $ do
    -- Prepare our socket
    receiver <- socket Pull
    bind receiver "tcp://*:5558"

    -- Wait for start of batch
    _ <- receive receiver

    -- Start our clock now
    start_time <- liftIO getCurrentTime

    -- Process 100 confirmations
    liftIO $ hSetBuffering stdout NoBuffering
    forM_ [1..100] $ \i -> do
        _ <- receive receiver
        if i `mod` 10 == 0
            then liftIO $ putStr ":"
            else liftIO $ putStr "."

    -- Calculate and report duration of batch
    end_time <- liftIO getCurrentTime
    liftIO . putStrLn $ "Total elapsed time: " ++ show (diffUTCTime end_time start_time * 1000) ++ " msec"
