{-# LANGUAGE OverloadedStrings #-}

--  Task worker - design 2
--  Adds pub-sub flow to receive and respond to kill signal

module Main where

import           Control.Concurrent
import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import           Data.Function
import           System.IO
import           System.ZMQ4.Monadic
import           Text.Printf

main :: IO ()
main = runZMQ $ do
    -- Socket to receive messages on
    receiver <- socket Pull
    connect receiver "tcp://localhost:5557"

    -- Socket to send messages to
    sender <- socket Push
    connect sender "tcp://localhost:5558"

    controller <- socket Sub
    connect controller "tcp://localhost:5559"
    subscribe controller ""

    liftIO $ hSetBuffering stdout NoBuffering

    fix $ \loop -> do
        [receiver_events, controller_events] <-
            poll (-1) [ Sock receiver   [In] Nothing
                      , Sock controller [In] Nothing
                      ]

        when (receiver_events /= []) $ do
            string <- BS.unpack <$> receive receiver
            liftIO $ printf "%s." string              -- Show the progress
            liftIO $ threadDelay (read string * 1000) -- Do the work
            send sender [] ""                         -- Send results to sink

        -- Any waiting controller command acts as 'KILL'
        unless (controller_events /= [])
            loop
