{-# LANGUAGE OverloadedStrings #-}

--  Task worker
--  Connects PULL socket to tcp://localhost:5557
--  Collects workloads from ventilator via that socket
--  Connects PUSH socket to tcp://localhost:5558
--  Sends results to sink via that socket

module Main where

import           Control.Concurrent
import           Control.Monad
import           Data.Monoid
import qualified Data.ByteString.Char8 as BS
import           System.IO
import           System.ZMQ4.Monadic

main :: IO ()
main = runZMQ $ do
    -- Socket to receive messages on
    receiver <- socket Pull
    connect receiver "tcp://localhost:5557"

    -- Socket to send messages to
    sender <- socket Push
    connect sender "tcp://localhost:5558"

    -- Process tasks forever
    forever $ do
        string <- receive receiver
        liftIO $ do
            BS.putStr (string <> ".")
            hFlush stdout
            threadDelay $ read (BS.unpack string) * 1000
        send sender [] ""
