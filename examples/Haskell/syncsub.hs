{-# LANGUAGE OverloadedStrings #-}
-- |
-- Node coordination Subscriber
-- 
module Main where
    

import System.ZMQ3.Monadic (runZMQ, socket, connect, receive, send, subscribe, Sub(..), Req(..), liftIO)
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

        liftIO $ putStrLn "[Subscriber] Send a synchronization request"
        send syncclient [] ""
        liftIO $ putStrLn "[Subscriber] Wait for reply"
        receive syncclient
        
        liftIO $ putStrLn "[Subscriber] Get update on the pub/sub channel"
        nbr <- countUpdates subscriber
        liftIO $ printf "[Subscriber] Received %d updates\n" (nbr ::Int)

    where
        countUpdates = loop 0 where
            loop val sock = do
                msg <- receive sock
                if unpack msg == "END"
                then return val 
                else do loop(val + 1) sock