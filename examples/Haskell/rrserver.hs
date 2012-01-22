-- |
-- Hello World server in Haskell
-- Binds REP socket to tcp://*:5560
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
  withSocket context Rep $ \responder -> do
    connect responder "tcp://localhost:5560"
  
    forever $ do
      message <- receive responder []
      putStrLn $ unwords ["Received request:", unpack message]    
    
      -- Simulate doing some 'work' for 1 second
      threadDelay (1 * 1000 * 1000)

      send responder reply []
      
  where reply = pack "World"
