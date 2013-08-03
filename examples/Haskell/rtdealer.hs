{-# LANGUAGE OverloadedStrings #-}
-- |
-- Router broker and DEALER workers (p.94)

module Main where

import System.ZMQ4.Monadic

import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.MVar (withMVar, newMVar, MVar)
import Data.ByteString.Char8 (unpack)
import Control.Monad (replicateM_, unless)
import ZHelpers (setRandomIdentity)
import Text.Printf
import Data.Time.Clock
import System.Random

nbrWorkers :: Int
nbrWorkers = 10

-- In general, although locks are an antipattern in ZeroMQ, we need a lock
-- for the stdout handle, otherwise we will get jumbled text. We don't 
-- use the lock for anything zeroMQ related, just output to screen.

workerThread :: MVar () -> IO ()
workerThread lock = 
    runZMQ $ do
        worker <- socket Dealer
        setRandomIdentity worker
        connect worker "ipc://routing.ipc"
        
        work worker

        where
            work = loop 0 where
                loop val sock = do
                    -- Send an empty frame manually
                    -- Unlike the Request socket, the Dealer does not it automatically
                    send sock [SendMore] ""
                    send sock [] "Ready"
                    -- unlike the Request socket we need to read the empty frame
                    receive sock
                    workload <- receive sock
                    if unpack workload == "Fired!"
                    then liftIO $ withMVar lock $ \_ -> printf "Completed: %d tasks\n" (val::Int)
                    else do
                        rand <- liftIO $ getStdRandom (randomR (500 :: Int, 5000))
                        liftIO $ threadDelay rand
                        loop (val+1) sock
    
main :: IO ()
main = 
    runZMQ $ do
        client <- socket Router
        bind client "ipc://routing.ipc"
        
        -- We only Need the MVar For Printing the Output (so output doesn't become interleaved)
        -- The alternative is to Make an ipc channel, but that distracts from the example
        -- Another alternative is to 'NoBuffering' 'stdin' and press Ctr-C manually
        lock <- liftIO $ newMVar ()

        liftIO $ replicateM_ nbrWorkers (forkIO $ workerThread lock)

        start <- liftIO getCurrentTime
        sendWork client start

        -- You need to give some time to the workers so they can exit properly
        liftIO $ threadDelay $ 1 * 1000 * 1000

    where
        sendWork :: Socket z Router -> UTCTime -> ZMQ z ()
        sendWork = loop nbrWorkers where
            loop c sock start = unless (c <= 0) $ do
                -- Next message is the leaset recently used worker
                ident <- receive sock
                send sock [SendMore] ident
                -- Envelope delimiter
                receive sock
                -- Ready signal from worker
                receive sock

                -- Send delimiter
                send sock [SendMore] ""
                -- Send Work unless time is up
                now <- liftIO getCurrentTime
                if c /= nbrWorkers || diffUTCTime now start > 5
                    then do
                        send sock [] "Fired!"
                        loop (c-1) sock start 
                    else do
                        send sock [] "Work harder"
                        loop c sock start