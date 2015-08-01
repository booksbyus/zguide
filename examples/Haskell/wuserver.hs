{-# LANGUAGE ScopedTypeVariables #-}

--  Weather update server
--  Binds PUB socket to tcp://*:5556
--  Publishes random weather updates

module Main where

import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import           System.Random
import           System.ZMQ4.Monadic
import           Text.Printf

main :: IO ()
main = runZMQ $ do
    -- Prepare our publisher
    publisher <- socket Pub
    bind publisher "tcp://*:5556"

    forever $ do
        -- Get values that will fool the boss
        zipcode     :: Int <- liftIO $ randomRIO (0, 100000)
        temperature :: Int <- liftIO $ randomRIO (-30, 135)
        relhumidity :: Int <- liftIO $ randomRIO (10, 60)

        -- Send message to all subscribers
        let update = printf "%05d %d %d" zipcode temperature relhumidity
        send publisher [] (BS.pack update)
