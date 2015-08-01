{-# LANGUAGE OverloadedStrings #-}

-- Hello World client

module Main where

import Control.Monad
import System.ZMQ4.Monadic

main :: IO ()
main = runZMQ $ do
    liftIO $ putStrLn "Connecting to hello world server..."

    requester <- socket Req
    connect requester "tcp://localhost:5555"

    forM_ [1..10] $ \i -> do
        liftIO . putStrLn $ "Sending Hello " ++ show i ++ "..."
        send requester [] "Hello"
        _ <- receive requester
        liftIO . putStrLn $ "Received World " ++ show i
