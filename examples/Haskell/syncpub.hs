{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.ZMQ3.Monadic (runZMQ, socket, bind, receive, send, Pub(..), Rep(..))
import Control.Monad (replicateM_, unless) 
    
subscribersExpected :: Int
subscribersExpected = 2
    
main :: IO ()
main = 
    runZMQ $ do
        
        publisher <- socket Pub
        bind publisher "tcp://*:5561"

        syncservice <- socket Rep
        bind syncservice "tcp://*:5562"
        
        -- Get synchronization from subscribers
        sync syncservice
        
        replicateM_ 1000000 $ send publisher [] "Rhubarb"
        
        send publisher [] "END"

    where
        sync = loop 0 where
               loop num sock = unless (num >= subscribersExpected) $ do
                    receive sock
                    send sock [] ""
                    loop (num + 1) sock