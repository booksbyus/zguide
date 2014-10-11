{--
Simple Pirate worker in Haskell
--}
module Main where

import System.ZMQ4.Monadic
import ZHelpers

import System.Random (randomRIO)
import System.Exit (exitSuccess)
import Control.Monad (when)
import Control.Concurrent (threadDelay)
import Control.Applicative ((<$>))    
import Data.ByteString.Char8 (pack, unpack, empty)


workerReady = "\001"

main :: IO ()
main = 
    runZMQ $ do
        worker <- socket Req
        setRandomIdentity worker
        connect worker "tcp://localhost:5556"

        id <- identity worker
        liftIO $ putStrLn $ "I: Worker ready " ++ unpack id
        send worker [SendMore] empty
        send worker [SendMore] empty
        send worker [] (pack workerReady)

        sendRequests worker 1

sendRequests :: Socket z Req -> Int -> ZMQ z ()
sendRequests worker cycles = do
    clID <- receive worker
    msg <- (receive worker >> receive worker)
    chance <- liftIO $ randomRIO (0::Int, 5)
    id <- identity worker
    if cycles > 3 && chance == 0
    then do
        liftIO $ putStrLn $ "I: Simulating a crash " ++ unpack id
        liftIO $ exitSuccess
    else do
        chance' <- liftIO $ randomRIO (0::Int, 5)
        when (cycles > 3 && chance' == 0) $ do
            liftIO $ putStrLn $ "I: Simulating overload " ++ unpack id
            liftIO $ threadDelay $ 3 * 1000 * 1000
    
    liftIO $ putStrLn $ "I: Normal reply " ++ unpack id
    liftIO $ threadDelay $ 1 * 1000 * 1000
    send worker [SendMore] clID
    send worker [SendMore] (pack "")
    send worker [] msg

    sendRequests worker (cycles+1)
