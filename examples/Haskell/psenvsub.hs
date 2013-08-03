{-# LANGUAGE OverloadedStrings #-}
-- |
-- Pub/Sub envelope subscriber (p.76)
-- 
module Main where
    
import System.ZMQ4.Monadic
import Data.ByteString.Char8 (unpack)
import Control.Monad (when)
import Text.Printf

main :: IO ()
main = 
    runZMQ $ do
        subscriber <- socket Sub
        connect subscriber "tcp://localhost:5563"
        subscribe subscriber "B"

        -- start listening for pub messages
        loop subscriber

    where
        loop subscriber = do
            -- read envelope with address
            receive subscriber >>= \addr -> liftIO $ printf "Address is %s\n" (unpack addr) 
            -- read message content
            more <- moreToReceive subscriber
            when more $ do
                contents <- receive subscriber 
                liftIO $ putStrLn (unpack contents)
            loop subscriber
