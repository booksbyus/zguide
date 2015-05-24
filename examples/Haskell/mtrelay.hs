{-# LANGUAGE OverloadedStrings #-}

--  Multithreaded relay

module Main where

import System.ZMQ4.Monadic

main :: IO ()
main = runZMQ step3

step3 :: ZMQ z ()
step3 = do
    -- Bind inproc socket before starting step2
    receiver <- socket Pair
    bind receiver "inproc://step3"
    async step2

    -- Wait for signal
    receive receiver

    liftIO $ putStrLn "Test successful!"

step2 :: ZMQ z ()
step2 = do
    -- Bind inproc socket before starting step1
    receiver <- socket Pair
    bind receiver "inproc://step2"
    async step1

    -- Wait for signal and pass it on
    receive receiver

    -- Connect to step 3 and tell it we're ready
    xmitter <- socket Pair
    connect xmitter "inproc://step3"
    liftIO $ putStrLn "Step 2 ready, signalling step3"
    send xmitter [] "READY"

step1 :: ZMQ z ()
step1 = do
    -- Connect to step2 and tell it we're ready
    xmitter <- socket Pair
    connect xmitter "inproc://step2"
    liftIO $ putStrLn "Step 1 ready, signalling step 2"
    send xmitter [] "READY"
