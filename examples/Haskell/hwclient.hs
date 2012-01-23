-- |
-- Hello World client in Haskell
-- Binds REQ socket to tcp://localhost:5555
-- Sends "Hello" to server, expects "World" back
-- 
-- Translated to Haskell by ERDI Gergo http://gergo.erdi.hu/

module Main where

import System.ZMQ
import Control.Monad (forM_)
import Data.ByteString.Char8 (pack, unpack)

main :: IO ()
main = withContext 1 $ \context -> do  
  putStrLn "Connecting to Hello World server..."  
  withSocket context Req $ \socket -> do
    connect socket "tcp://localhost:5555"
    forM_ [1..10] $ \i -> do
      putStrLn $ unwords ["Sending request", show i]
      send socket request []
    
      reply <- receive socket []
      putStrLn $ unwords ["Received reply:", unpack reply]    

  where request = pack "Hello"
