{-# LANGUAGE OverloadedStrings #-}
-- |
-- A worker that simulates some work with a timeout
-- And send back "World"
-- Connect REP socket to tcp://*:5560
-- Expects "Hello" from client, replies with "World"
-- 
-- Originally translated to Haskell by ERDI Gergo http://gergo.erdi.hu/

module Main where

import System.ZMQ3.Monadic (runZMQ, socket, connect, send, receive, Rep(..), liftIO)
import Control.Monad (forever)
import Data.ByteString.Char8 (unpack)
import Control.Concurrent (threadDelay)
import Text.Printf

main :: IO ()
main = 
    runZMQ $ do
        responder <- socket Rep
        connect responder "tcp://localhost:5560"
  
        forever $ do
            receive responder >>= liftIO . printf "Received request: [%s]\n" . unpack
            -- Simulate doing some 'work' for 1 second
            liftIO $ threadDelay (1 * 1000 * 1000)
            send responder [] "World"       
        
        



    
