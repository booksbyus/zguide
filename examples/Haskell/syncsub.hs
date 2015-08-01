{-# LANGUAGE OverloadedStrings #-}

--  Synchronized subscriber

module Main where

import Control.Concurrent
import Data.Function
import System.ZMQ4.Monadic
import Text.Printf

main :: IO ()
main = runZMQ $ do
    -- First, connect our subscriber socket
    subscriber <- socket Sub
    connect subscriber "tcp://localhost:5561"
    subscribe subscriber ""

    -- 0MQ is so fast, we need to wait a while...
    liftIO $ threadDelay 1000000

    -- Second, synchronize with the publisher
    syncclient <- socket Req
    connect syncclient "tcp://localhost:5562"

    -- Send a synchronization request
    send syncclient [] ""

    -- Wait for a synchronization reply
    receive syncclient

    let -- go :: (Int -> ZMQ z Int) -> Int -> ZMQ z Int
        go loop = \n -> do
            string <- receive subscriber
            if string == "END"
                then return n
                else loop (n+1)

    -- Third, get our updates and report how many we got
    update_nbr <- fix go (0 :: Int)
    liftIO $ printf "Received %d updates\n" update_nbr
