{-# LANGUAGE OverloadedStrings #-}

--  Synchronized publisher

module Main where

import Control.Monad
import System.ZMQ4.Monadic

subscribers_expected :: Int
subscribers_expected = 10

main :: IO ()
main = runZMQ $ do
    -- Socket to talk to clients
    publisher <- socket Pub
    setSendHighWM (restrict 1100000) publisher
    bind publisher "tcp://*:5561"

    -- Socket to receive signals
    syncservice <- socket Rep
    bind syncservice "tcp://*:5562"

    -- Get synchronization from subscribers
    liftIO $ putStrLn "Waiting for subscribers"
    replicateM_ subscribers_expected $ do
        receive syncservice
        send syncservice [] ""

    -- Now broadcast exactly 1M updates followed by END
    liftIO $ putStrLn "Broadcasting messages"
    replicateM_ 1000000 (send publisher [] "Rhubarb")
    send publisher [] "END"
