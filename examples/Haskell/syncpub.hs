{-# LANGUAGE OverloadedStrings #-}
-- |
-- Node coordination Publisher p.71
-- 
-- To test this example, you have to fire as much 'syncsub' as expected. 

module Main where

import System.ZMQ4.Monadic 
import Control.Monad (replicateM_, unless) 
import System.IO (hSetBuffering, stdout, BufferMode(..))
import Data.ByteString.Char8 (unpack)
import Text.Printf

subscribersExpected = 4 
nbOfUpdate = 10000

-- | Wait for the expected subscribers to signal their presence.
sync :: Socket z Rep -> ZMQ z ()
sync = loop 0
    where
       loop num sock =
            unless (num >= subscribersExpected) $ do
                receive sock >>= \msg -> liftIO $ printf "[Publisher receives] %s\n" (unpack msg)
                send sock [] ""
                loop (num + 1) sock

main :: IO ()
main = 
    runZMQ $ do
        
        publisher <- socket Pub
        -- if we don't set the HWM, 
        -- pub update above 1000 (default HWM) might not be processed by subs before the termination signal
        setSendHighWM (restrict nbOfUpdate) publisher
        bind publisher "tcp://*:5561"

        -- socket to receive signals
        syncservice <- socket Rep
        bind syncservice "tcp://*:5562"
        
        liftIO $ hSetBuffering stdout NoBuffering
        liftIO $ putStrLn "[Publisher] Get synchronization from subscribers"
        sync syncservice
        
        liftIO $ putStrLn "[Publisher] Send updates to subscribers"
        replicateM_ nbOfUpdate $ send publisher [] "Rhubarb"

        liftIO $ putStrLn "[Publisher] Send termination signal"
        send publisher [] "END"