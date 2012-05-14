-- |
-- Hello World client in Haskell
-- Binds REQ socket to tcp://localhost:5559
-- Sends "Hello" to server, expects "World" back
-- 
-- Translated to Haskell by ERDI Gergo http://gergo.erdi.hu/

module Main where

import System.ZMQ
import Control.Monad (forM_)
import Data.ByteString.Char8 (pack, unpack)

main :: IO ()
main = withContext 1 $ \context -> do  
  withSocket context Req $ \requester -> do
    connect requester "tcp://localhost:5559"
    forM_ [1..10] $ \i -> do
      send requester request []    
      reply <- receive requester []
      putStrLn $ unwords ["Received reply", show i, unpack reply]

  where request = pack "Hello"
