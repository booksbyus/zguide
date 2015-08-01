{-# LANGUAGE OverloadedStrings #-}

--  Task sink - design 2
--  Adds pub-sub flow to send kill signal to workers

module Main where

import Control.Monad
import Data.Time.Clock
import System.IO
import System.ZMQ4.Monadic

main :: IO ()
main = runZMQ $ do
    -- Socket to receive messages on
    receiver <- socket Pull
    bind receiver "tcp://*:5558"

    -- Socket for worker control
    controller <- socket Pub
    bind controller "tcp://*:5559"

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

    end_time <- liftIO getCurrentTime
    liftIO . putStrLn $ "Total elapsed time: " ++ show (diffUTCTime end_time start_time * 1000) ++ " msec"

    -- Send kill signal to workers
    send controller [] "KILL"
