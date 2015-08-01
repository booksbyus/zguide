{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

--  Weather update client
--  Connects SUB socket to tcp://localhost:5556
--  Collects weather updates and finds avg temp in zipcode

module Main where

import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import           System.Environment
import           System.ZMQ4.Monadic
import           Text.Printf

main :: IO ()
main = runZMQ $ do
    liftIO $ putStrLn "Collecting updates from weather server..."

    -- Socket to talk to server
    subscriber <- socket Sub
    connect subscriber "tcp://localhost:5556"

    -- Subscribe to zipcode, default is NYC, 10001
    filter <- liftIO getArgs >>= \case
        []          -> return "10001 "
        (zipcode:_) -> return (BS.pack zipcode)
    subscribe subscriber filter

    -- Process 100 updates
    temperature <- fmap sum $
        replicateM 100 $ do
            string <- receive subscriber
            let [_, temperature :: Int, _] = map read . words . BS.unpack $ string
            return temperature

    liftIO $
        printf "Average temperature for zipcode '%s' was %dF"
               (BS.unpack filter)
               (temperature `div` 100)
