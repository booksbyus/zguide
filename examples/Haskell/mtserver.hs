{-# LANGUAGE OverloadedStrings #-}
-- |
-- Multithreaded Hello World server in Haskell
-- 
-- Translated to Haskell by ERDI Gergo http://gergo.erdi.hu/

module Main where

import System.ZMQ3.Monadic
import Control.Monad (forever, replicateM_)
import Data.ByteString.Char8 (unpack)
import Control.Concurrent (threadDelay)
import Text.Printf


main :: IO ()
main =
    runZMQ $ do
        -- Socket to talk to clients
        clients <- socket Router
        bind clients "tcp://*:5555"
    
        -- Socket to talk to workers
        workers <- socket Dealer
        bind workers "inproc://workers"
      
        -- using inproc (inter-thread) we expect to share the same context
        replicateM_ 5 (async worker)
        
        -- Connect work threads to client threads via a queue
        proxy clients workers Nothing

worker :: ZMQ z ()
worker = do
    receiver <- socket Rep
    connect receiver "inproc://workers"
    forever $ do
      receive receiver >>= liftIO . printf "Received request:%s\n" . unpack    
      -- Simulate doing some 'work' for 1 second
      liftIO $ threadDelay (1 * 1000 * 1000)  
      send receiver [] "World"
