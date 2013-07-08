{-# LANGUAGE OverloadedStrings #-}
-- |
-- Node coordination Publisher
-- 
module Main where

import System.ZMQ3.Monadic (runZMQ, socket, bind, receive, send, Pub(..), Rep(..), liftIO)
import Control.Monad (replicateM_, unless) 
import System.IO (hSetBuffering, stdout, BufferMode(..))
import Control.Concurrent (threadDelay)

subscribersExpected :: Int
subscribersExpected = 5 
    
main :: IO ()
main = 
    runZMQ $ do
        
        publisher <- socket Pub
        bind publisher "tcp://*:5561"

        syncservice <- socket Rep
        bind syncservice "tcp://*:5562"
        

        liftIO $ hSetBuffering stdout NoBuffering
        liftIO $ putStrLn "[Publisher] Get synchronization from subscribers"
        sync syncservice
        
        liftIO $ putStrLn "[Publisher] Send updates to subscribers"
        replicateM_ 5000 $ send publisher [] "Rhubarb"
        
        liftIO $ putStrLn "[Publisher] Send termination signal"
        send publisher [] "END"

    where
        sync = loop 0 where
               loop num sock = unless (num >= subscribersExpected) $ do
                    receive sock
                    send sock [] ""
                    loop (num + 1) sock