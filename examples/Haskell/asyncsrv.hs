module Main where

import System.ZMQ
import ZHelpers (setId)
import Control.Concurrent (forkIO, threadDelay)
import Data.ByteString.Char8 (pack, unpack)
import Control.Monad (forever, forM_, when, replicateM_)
import System.Random (newStdGen, randomR, StdGen)

clientTask :: Context -> IO ()
clientTask ctx = withSocket ctx XReq $ \client -> do
    setId client
    connect client "tcp://localhost:5570"
    forever $ do
        forM_ [0..5] $ \centitick -> do
            [S client' status] <- poll [S client In] 1000
            when (status == In) $ do
                msg <- receive client []
                putStrLn "Client"
        putStrLn "Req Sent"
        send client (pack "request") []
        
serverTask :: Context -> IO ()
serverTask ctx = withSocket ctx XRep $ \frontend -> do
    bind frontend "tcp://*:5570"
    withSocket ctx XReq $ \backend -> do
        bind backend "inproc://backend"
        replicateM_ 5 (forkIO $ serverWorker ctx)
        [S frontend' res1, S backend' res2] <- poll [S frontend In, S backend In] (-1)
        when (res1 == In) $ do
            _id <- receive frontend []
            msg <- receive frontend []
            print "Sending"
            send backend _id [SndMore]
            send backend msg []
                        
        when (res2 /= In) $ do
            _id <- receive backend []
            msg <- receive backend []
            print "Sending"
            send frontend _id [SndMore]
            send frontend msg []

serverWorker :: Context -> IO ()
serverWorker ctx = withSocket ctx XReq $ \worker -> do
    connect worker "inproc://backend"
    putStrLn "Worker Started"
    forever $ do
        _id <- receive worker []
        msg <- receive worker []
        putStrLn "Worker Received Data"
        gen <- newStdGen
        let (val, gen') = randomR (0,5) gen :: (Int, StdGen)
        forM_ [1..val] $ \i -> do
            threadDelay $ 1 * 1000 * 1000
            send worker _id [SndMore]
            send worker msg []

main :: IO ()
main = withContext 1 $ \context -> do
    forkIO (clientTask context)
    forkIO (clientTask context)
    forkIO (clientTask context)
    forkIO (serverTask context)
    
    threadDelay $ 5 * 1000 * 1000