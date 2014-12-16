{-
    Round-trip demonstrator.
    Runs as a single process for easier testing.
    The client task signals to main when it's ready.
-}
import System.ZMQ4

import Control.Concurrent
import Control.Monad (forM_, forever)
import Data.ByteString.Char8 (pack, unpack)
import Data.Time.Clock (getCurrentTime, diffUTCTime)


client_task :: Context -> Socket Pair -> IO ()
client_task ctx pipe =
    withSocket ctx Dealer $ \client -> do
        connect client "tcp://localhost:5555"
        putStrLn "Setting up test..."
        threadDelay $ 100 * 1000

        putStrLn "Synchronous round-trip test..."
        start <- getCurrentTime
        forM_ [0..10000] $ \_ -> do
            send client [] (pack "hello")
            receive client
        end <- getCurrentTime
        let elapsed_ms = (diffUTCTime end start) * 1000
        putStrLn $ " " ++ (show $ (1000 * 10000) `quot` (round elapsed_ms)) ++ " calls/second"

        putStrLn "Asynchronous round-trip test..."
        start <- getCurrentTime
        forM_ [0..10000] $ \_ -> do
            send client [] (pack "hello")
        forM_ [0..10000] $ \_ -> do
            receive client
        end <- getCurrentTime
        let elapsed_ms = (diffUTCTime end start) * 1000
        putStrLn $ " " ++ (show $ (1000 * 10000) `quot` (round elapsed_ms)) ++ " calls/second"

        send pipe [] (pack "done")

worker_task :: IO ()
worker_task = 
    withContext $ \ctx ->
        withSocket ctx Dealer $ \worker -> do
            connect worker "tcp://localhost:5556"

            forever $ do
                msg <- receive worker
                send worker [] msg

broker_task :: IO ()
broker_task =
    withContext $ \ctx ->
        withSocket ctx Dealer $ \frontend ->
        withSocket ctx Dealer $ \backend -> do
            bind frontend "tcp://*:5555"
            bind backend "tcp://*:5556"

            proxy frontend backend Nothing

main :: IO ()
main =
    withContext $ \ctx ->
        withSocket ctx Pair $ \pipeServer ->
        withSocket ctx Pair $ \pipeClient -> do
            bind pipeServer "inproc://my-pipe"
            connect pipeClient "inproc://my-pipe"

            forkIO (client_task ctx pipeClient)
            forkIO worker_task
            forkIO broker_task

            signal <- receive pipeServer
            putStrLn $ unpack signal
