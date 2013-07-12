{-# LANGUAGE OverloadedStrings #-}
-- |
-- Node coordination Subscriber
-- 
module Main where
    

import System.ZMQ3.Monadic (ZMQ, Socket, runZMQ, socket, connect, receive, send, subscribe, Sub(..), Req(..), liftIO)
import Data.ByteString.Char8 (unpack)
import Control.Concurrent (threadDelay)
import Text.Printf

main :: IO ()
main = 
    runZMQ $ do
        -- setup the sub channel
        subscriber <- socket Sub
        connect subscriber "tcp://localhost:5561"
        subscribe subscriber ""

        -- 0mq is fast, we need to wait for a while ...
        liftIO $ threadDelay $ 1 * 1000 * 1000

        -- set up sync channel
        syncclient <- socket Req
        connect syncclient "tcp://localhost:5562"

        send syncclient [] "synchronization request from subscriber"
        -- wait for synchronization reply
        receive syncclient
        
        -- get update on the pub/sub channel"
        countUpdates subscriber >>= liftIO . printf "[Subscriber] Received %d updates\n"


    where
        countUpdates :: Socket z Sub -> ZMQ z (Int) 
        countUpdates = loop 0 where
            loop val sock = do
                msg <- receive sock
                if unpack msg == "END"
                then return val 
                else do loop(val + 1) sock