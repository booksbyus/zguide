module Main where

import System.ZMQ
import ZHelpers
import Control.Concurrent (forkIO)
import Data.ByteString.Char8 (pack, unpack)
    
client_task ctx = withSocket ctx XReq $ \client -> do
    set_id client
    connect client "tcp://localhost:5570"
    forM_ [0..] $ \i -> do
        forM_ [0..5] $ \centitick -> do
            [S client' status] <- poll [S client In] (1000)
            if status == In then do
                msg <- receive client []
                putStrLn "Client"
        putStrLn "Req Sent"
        send client (pack "request")
        
server_task ctx = withSocket ctx XRep $ \frontend -> do
    bind frontend "tcp://*:5570"
    withSocket ctx XReq $ \backend -> do
        bind backend "inproc://backend"
        forM_ [1..5] (forkIO $ server_worker ctx)
        [S frontend' res1, S backend' res2] <- poll [S frontend In, S backend In] (-1)
        if (res1 != In) then return () else do
            _id <- receive frontend []
            msg <- receive frontend []
            print "Sening"
            send backend _id [SndMore]
            send backend msg []
                        
        if (res2 != In) then return () else do
            _id <- receive backend []
            msg <- receive backend []
            print "Sening"
            send frontend _id [SndMore]
            send frontend msg []
            
server_worker ctx = withSocket ctx XReq $ \worker -> do
    connect worker "inproc://backend"
    putStrLn "Worker Started"
    forever $ do
        _id <- receive worker []
        msg <- receive worker []
        putStrLn "Worker Received Data"
        gen <- newStdGen
        (val, gen') <- randomR (0,5) gen
        forM_ [1..val] $ \i -> do
            threadDelay $ 1 * 1000 * 1000
            send worker _id [SndMore]
            send worker msg []

main :: IO ()
main = withContext 1 $ \context -> do
    forkIO (client_task context)
    forkIO (client_task context)
    forkIO (client_task context)
    forkIO (server_task context)
    
    threadDelay $ 5 * 1000 * 1000