{--
Lazy Pirate client in Haskell
--}
module Main where

import System.ZMQ4.Monadic
import System.Random (randomRIO)
import System.Exit (exitSuccess)
import Control.Monad (forever, when)
import Control.Concurrent (threadDelay)
import Data.ByteString.Char8 (pack, unpack)


requestRetries = 3
requestTimeout_ms = 2500
serverEndpoint = "tcp://localhost:5555"

main :: IO ()
main =
    runZMQ $ do
        liftIO $ putStrLn "I: Connecting to server"
        client <- socket Req
        connect client serverEndpoint

        sendServer 1 requestRetries client

sendServer :: Int -> Int -> Socket z Req -> ZMQ z ()
sendServer _ 0 _ = return ()
sendServer seq retries client = do
    send client [] (pack $ show seq)

    pollServer seq retries client

pollServer :: Int -> Int -> Socket z Req -> ZMQ z ()
pollServer seq retries client = do
    [evts] <- poll requestTimeout_ms [Sock client [In] Nothing]

    if In `elem` evts
    then do
        reply <- receive client
        if (read . unpack $ reply) == seq
        then do
            liftIO $ putStrLn $ "I: Server replied OK " ++ (unpack reply)
            sendServer (seq+1) requestRetries client
        else do
            liftIO $ putStrLn $ "E: malformed reply from server: " ++ (unpack reply)
            pollServer seq retries client
    else
        if retries == 0
        then liftIO $ putStrLn "E: Server seems to be offline, abandoning" >> exitSuccess
        else do
            liftIO $ putStrLn $ "W: No response from server, retrying..."
            client' <- socket Req
            connect client' serverEndpoint
            send client' [] (pack $ show seq)
            pollServer seq (retries-1) client'
