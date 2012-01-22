-- |
-- Multithreaded Hello World server in Haskell
-- 
-- Translated to Haskell by ERDI Gergo http://gergo.erdi.hu/

module Main where

import System.ZMQ
import Control.Monad (forever, replicateM_)
import Data.ByteString.Char8 (pack, unpack)
import Control.Concurrent (threadDelay, forkIO)

worker :: Context -> IO ()
worker context = do
  withSocket context Rep $ \receiver -> do
    connect receiver "inproc://workers"
    forever $ do
      message <- receive receiver []
      putStrLn $ unwords ["Received request:", unpack message]    
      -- Simulate doing some 'work' for 1 second
      threadDelay (1 * 1000 * 1000)  
      send receiver reply []
  where reply = pack "World"

main :: IO ()
main = withContext 1 $ \context -> do  
  -- Socket to talk to clients
  withSocket context XRep $ \clients -> do
    bind clients "tcp://*:5555"
    
    -- Socket to talk to workers
    withSocket context XReq $ \workers -> do
      bind workers "inproc://workers"
      
      replicateM_ 5 $ forkIO (worker context)
        
      -- Connect work threads to client threads via a queue
      device Queue clients workers
