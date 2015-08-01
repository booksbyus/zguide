{-# LANGUAGE OverloadedStrings #-}

--  Pubsub envelope subscriber

module Main where

import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import           System.ZMQ4.Monadic
import           Text.Printf

main :: IO ()
main = runZMQ $ do
    -- Prepare our subscriber
    subscriber <- socket Sub
    connect subscriber "tcp://localhost:5563"
    subscribe subscriber "B"

    forever $ do
        -- Read envelope with address
        address <- receive subscriber
        -- Read message contents
        contents <- receive subscriber
        liftIO $ printf "[%s] %s\n" (BS.unpack address) (BS.unpack contents)
