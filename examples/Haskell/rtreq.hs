module Main where

import System.ZMQ
import ZHelpers (setId)

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (withMVar, newMVar, MVar)
import Data.ByteString.Char8 (pack, unpack)
import Control.Monad (replicateM_)

nbrWorkers :: Int
nbrWorkers = 10

-- In general, although locks are an antipattern in ZeroMQ, we need a lock
-- for the stdout handle, otherwise we will get jumbled text. We don't 
-- use the lock for anything zeroMQ related, just output to screen.

workerThread :: Context -> MVar () -> IO ()
workerThread ctx lock = withSocket ctx Req $ \worker -> do
    setId worker
    connect worker "ipc://routing.ipc"
    let work_func val = do
        send worker (pack "ready") []
        workload <- receive worker []
        if unpack workload == "END"
            then withMVar lock $ \_ -> putStrLn $ "Processed: " ++ show val ++ " tasks"
            else work_func (val+1)
    work_func 0
    
main :: IO ()
main = withContext 1 $ \context -> do
    withSocket context XRep $ \client -> do
        bind client "ipc://routing.ipc"
        
        -- We only Need the MVar For Printing the Output (so output doesn't become interleaved)
        -- The alternative is to Make an ipc channel, but that distracts from the example.
        lock <- newMVar ()
        
        replicateM_ nbrWorkers (forkIO (workerThread context lock))

        let generalProc a = do
            address <- receive client []
            empty <- receive client []
            ready <- receive client []
            
            send client address [SndMore]
            send client (pack "") [SndMore]
            send client (pack a) []

        replicateM_ (nbrWorkers * 10) (generalProc "This is the workload")
        replicateM_ nbrWorkers (generalProc "END")

        threadDelay $ 1 * 1000 * 1000