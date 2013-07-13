{-# LANGUAGE OverloadedStrings #-}
-- |
-- Pub/Sub envelope subscriber
-- 
module Main where
    
import System.ZMQ3.Monadic (runZMQ, socket, connect, subscribe, receive, moreToReceive, Sub(..), liftIO)
import Data.ByteString.Char8 (unpack)
import Control.Monad (when)
import Text.Printf

main :: IO ()
main = 
    runZMQ $ do
        subscriber <- socket Sub
        connect subscriber "tcp://localhost:5563"
        subscribe subscriber "B"
        -- read envelope with address
        receive subscriber >>= \addr -> liftIO $ printf "Address is %s\n" (unpack addr)
        --  loop contents
        loop subscriber
    where
        loop subscriber = do
            more <- moreToReceive subscriber
            when more $ do
                contents <- receive subscriber 
                liftIO $ putStrLn (unpack contents)
                loop subscriber
