{-
Paranoid Pirate worker in Haskell.
Uses heartbeating to detect crashed queue.
-}
module Main where

import System.ZMQ4.Monadic
import ZHelpers

import System.Random (randomRIO)
import System.Exit (exitSuccess)
import System.IO (hSetEncoding, stdout, utf8)
import Control.Monad (when)
import Control.Concurrent (threadDelay)
import Data.ByteString.Char8 (pack, unpack, empty)


heartbeatLiveness = 3
heartbeatInterval_ms = 1000 :: Integer
reconnectIntervalInit = 1000
reconnectIntervalLimit = 32000

pppReady = pack "\001"
pppHeartbeat = pack "\002"


createWorkerSocket :: ZMQ z (Socket z Dealer)
createWorkerSocket = do
    worker <- socket Dealer
    connect worker "tcp://localhost:5556"
    
    liftIO $ putStrLn "I: worker ready"
    send worker [] pppReady

    return worker

main :: IO ()
main = 
    runZMQ $ do
        worker <- createWorkerSocket
        heartbeatAt <- liftIO $ nextHeartbeatTime_ms heartbeatInterval_ms
        
        liftIO $ hSetEncoding stdout utf8
        pollWorker worker heartbeatAt heartbeatLiveness reconnectIntervalInit 0

pollWorker :: Socket z Dealer -> Integer -> Integer -> Integer -> Int -> ZMQ z ()
pollWorker worker heartbeat liveness reconnectInterval cycles = do
    [evts] <- poll (fromInteger heartbeatInterval_ms) [Sock worker [In] Nothing]

    if In `elem` evts
    then do
        frame <- receiveMulti worker

        if length frame == 4 -- Handle normal message
        then do
            chance <- liftIO $ randomRIO (0::Int, 5)
            if cycles > 3 && chance == 0
            then do
                liftIO $ putStrLn "I: Simulating a crash"
                liftIO $ exitSuccess
            else do
                chance' <- liftIO $ randomRIO (0::Int, 5)
                when (cycles > 3 && chance' == 0) $ do
                    liftIO $ putStrLn "I: Simulating CPU overload"
                    liftIO $ threadDelay $ 3 * 1000 * 1000

            let id = frame !! 1
                info = frame !! 3
            liftIO $ putStrLn "I: Normal reply" 
            send worker [SendMore] id
            send worker [SendMore] empty
            send worker [] info
            liftIO $ threadDelay $ 1 * 1000 * 1000

            pollWorker worker heartbeat heartbeatLiveness reconnectIntervalInit (cycles+1)
        else if length frame == 2 -- Handle heartbeat or eventual invalid message
            then do
                let msg = frame !! 1
                if msg == pppHeartbeat
                then pollWorker worker heartbeat heartbeatLiveness reconnectIntervalInit cycles
                else do
                    liftIO $ putStrLn "E: invalid message" 
                    pollWorker worker heartbeat liveness reconnectIntervalInit cycles
            else do
                liftIO $ putStrLn "E: invalid message"
                pollWorker worker heartbeat liveness reconnectIntervalInit cycles
    else do
        when (liveness == 0) $ do -- Try to reconnect
            liftIO $ putStrLn "W: heartbeat failure, can't reach queue"
            liftIO $ putStrLn $ "W: reconnecting in " ++ (show reconnectInterval) ++ " msec..."
            liftIO $ threadDelay $ (fromInteger reconnectInterval) * 1000

            worker' <- createWorkerSocket
            if (reconnectInterval < reconnectIntervalLimit)
            then pollWorker worker' heartbeat heartbeatLiveness (reconnectInterval * 2) cycles
            else pollWorker worker' heartbeat heartbeatLiveness reconnectInterval cycles

        currTime <- liftIO $ currentTime_ms -- Send heartbeat
        when (currTime > heartbeat) $ do
            liftIO $ putStrLn "I: worker heartbeat"
            send worker [] pppHeartbeat
            newHeartbeat <- liftIO $ nextHeartbeatTime_ms heartbeat
            pollWorker worker newHeartbeat (liveness-1) reconnectInterval cycles
        pollWorker worker heartbeat (liveness-1) reconnectInterval cycles
