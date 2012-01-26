-- |
-- Simple message queuing broker in Haskell
-- Same as request-reply broker but using QUEUE device
-- 
-- Translated to Haskell by ERDI Gergo http://gergo.erdi.hu/

module Main where

import System.ZMQ
import Control.Monad (forever, when)
import Data.Function (fix)
import Data.ByteString.Char8 (pack, unpack)
import Control.Concurrent (threadDelay)

main :: IO ()
main = withContext 1 $ \context -> do
  withSocket context XRep $ \frontend -> do
    withSocket context XReq $ \backend -> do
      
      bind frontend "tcp://*:5559"
      bind backend "tcp://*:5560"      
      device Queue frontend backend
