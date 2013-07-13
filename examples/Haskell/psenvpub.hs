{-# LANGUAGE OverloadedStrings #-}
-- |
-- Pub/Sub envelope publisher
-- 
module Main where

import System.ZMQ3.Monadic (runZMQ, socket, bind, send, Pub(..), Flag(SendMore), liftIO)
import Control.Concurrent (threadDelay)
import Control.Monad (forever)

main :: IO ()
main = 
    runZMQ $ do
        publisher <- socket Pub
        bind publisher "tcp://*:5563"
        forever $ do
            send publisher [SendMore] "A"
            send publisher [] "We don't want to see this"
            send publisher [SendMore] "B" 
            send publisher [] "We would like to see this" 
            liftIO $ threadDelay $ 1 * 1000 * 1000