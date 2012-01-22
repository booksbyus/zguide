module Main where

import System.ZMQ
import ZHelpers

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Data.ByteString.Char8 (pack, unpack)
import Control.Monad (forM_)

nbr_workers :: Int
nbr_workers = 10
    
worker_thread ctx lock = withSocket ctx Req $ \worker -> do
    set_id worker
    connect worker "ipc://routing.ipc"
    let work_func val = do
        send worker (pack "ready") []
        workload <- receive worker []
        if (unpack workload == "END") then (withMVar lock $ \_ -> putStrLn $ "Processed: " ++ (show val) ++ " tasks")
            else work_func (val+1)
    work_func 0
    
main :: IO ()
main = withContext 1 $ \context -> do
    withSocket context XRep $ \client -> do
        bind client "ipc://routing.ipc"
        
        -- We only Need the MVar For Printing the Output (so output doesn't become interleaved)
        -- The alternative is to Make a New Channel (to be more 0MQish)
        lock <- newMVar ()
        
        forM_ [1..nbr_workers] (\_ -> forkIO (worker_thread context lock))

        forM_ [1..(nbr_workers * 10)] $ \_ -> do
            address <- receive client []
            empty <- receive client []
            ready <- receive client []
            
            send client address [SndMore]
            send client (pack "") [SndMore]
            send client (pack "This is the workload") []

        forM_ [1..nbr_workers] $ \_ -> do
            address <- receive client []
            empty <- receive client []
            ready <- receive client []
            
            send client address [SndMore]
            send client (pack "") [SndMore]
            send client (pack "END") []

        threadDelay $ 1 * 1000 * 1000