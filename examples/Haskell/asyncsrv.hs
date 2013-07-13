module Main where

import System.ZMQ3.Monadic
import ZHelpers (setId)
import Control.Concurrent (threadDelay)
import Data.ByteString.Char8 (pack, unpack)
import Control.Monad (forever, forM_, when, unless, replicateM_)
import System.Random (newStdGen, randomR, StdGen)

clientTask :: ZMQ z ()
clientTask = do
    client <- socket Dealer
    setId client
    connect client "tcp://localhost:5570"
    forever $ do
        forM_ [0..100] $ \centitick -> do
            [evts] <- poll 10 [Sock client [In] Nothing]
            when (In `elem` evts) $ do
                receive client >>= \msg -> liftIO $ putStrLn $ unwords ["Client received", (unpack msg), "back from worker"]
        liftIO $ putStrLn "Req Sent"
        send client [] (pack "request")

        
serverTask :: ZMQ z ()
serverTask =  do

    frontend <- socket Router
    bind frontend "tcp://*:5570"
    backend <- socket Dealer
    bind backend "inproc://backend"
    
    replicateM_ 5 $ async serverWorker

    -- proxy does not work here ?
    forever $ pollEvt frontend backend

    where
        pollEvt frontend backend = do 
            [evtsF, evtsB] <- poll (-1) [Sock frontend [In] Nothing, Sock backend [In] Nothing] 
            
            when (In `elem` evtsF) $ do
                receive frontend >>= send backend [SendMore]
                receive frontend >>= send backend []
                            
            unless (In `elem` evtsB) $ do
                receive backend >>= send frontend  [SendMore]
                receive backend >>= send frontend []



serverWorker :: ZMQ z ()
serverWorker = do
    worker <- socket Dealer
    connect worker "inproc://backend"
    liftIO $ putStrLn "Worker Started"        
    forever $ do
        _id <- receive worker
        msg <- receive worker
        liftIO $ putStrLn "Worker Received Data"
        gen <- liftIO $ newStdGen
        let (val, _) = randomR (0,5) gen :: (Int, StdGen)
        forM_ [1..val] $ \i -> do
            liftIO $ threadDelay $ 1 * 1000 * 1000
            send worker [SendMore] _id
            send worker [] msg

main :: IO ()
main = 
    runZMQ $ do
        async clientTask
        async clientTask
        async clientTask
        async serverTask
        
        liftIO $ threadDelay $ 5 * 1000 * 1000