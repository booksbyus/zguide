{-# LANGUAGE OverloadedStrings #-}
-- |
-- Weather broadcast server in Haskell
-- Binds PUB socket to tcp://*:5556
-- Publishes random weather updates
-- 
-- Translated to Haskell by ERDI Gergo http://gergo.erdi.hu/

module Main where

import System.ZMQ3.Monadic
import Control.Monad (forever)
import Data.ByteString.Char8 (pack)
import System.Random (randomRIO)

main :: IO ()
main =  runZMQ $ do
    publisher <- socket Pub 
    bind publisher "tcp://*:5556"
    bind publisher "ipc://weather.ipc"
  
    forever $ do
        zipcode <- liftIO $ (randomRIO (0, 100000) :: IO Int)
        temperature <- liftIO $ (randomRIO (-80, 135) :: IO Int)
        humidity <- liftIO $ (randomRIO (10, 60) :: IO Int)
      
        let update = pack $ unwords [show zipcode, show temperature, show humidity]
        send publisher [] update
