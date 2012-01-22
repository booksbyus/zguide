-- |
-- Hello World server in Haskell
-- Binds REP socket to tcp://*:5555
-- Expects "Hello" from client, replies with "World"
-- 
-- Translated to Haskell by ERDI Gergo http://gergo.erdi.hu/

module Main where

import System.ZMQ
import Control.Monad (forever)
import Data.ByteString.Char8 (pack, unpack)
import Control.Concurrent (threadDelay)

main :: IO ()
main = withContext 1 $ \context -> do  
  putStrLn "Starting Hello World server"
  withSocket context Rep $ \socket -> do
    bind socket "tcp://*:5555"
  
    putStrLn "Entering main loop"
    forever $ do
      message <- receive socket []
      putStrLn $ unwords ["Received request:", unpack message]    
    
      -- Simulate doing some 'work' for 1 second
      threadDelay (1 * 1000 * 1000)

      send socket (pack "World") []
