{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

--  Task ventilator
--  Binds PUSH socket to tcp://localhost:5557
--  Sends batch of tasks to workers via that socket

module Main where

import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import           System.ZMQ4.Monadic
import           System.Random

main :: IO ()
main = runZMQ $ do
    -- Socket to send messages on
    sender <- socket Push
    bind sender "tcp://*:5557"

    -- Socket to send start of batch message on
    sink <- socket Push
    connect sink "tcp://localhost:5558"

    liftIO $ do
        putStrLn "Press Enter when the workers are ready: "
        _ <- getLine
        putStrLn "Sending tasks to workers..."

    -- The first message is "0" and signals start of batch
    send sink [] "0"

    -- Send 100 tasks
    total_msec <- fmap sum $
        replicateM 100 $ do
            -- Random workload from 1 to 100msecs
            workload :: Int <- liftIO $ randomRIO (1, 100)
            send sender [] $ BS.pack (show workload)
            return workload

    liftIO . putStrLn $ "Total expected cost: " ++ show total_msec ++ " msec"
