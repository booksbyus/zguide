-- |
-- Weather proxy device in Haskell
-- 
-- Translated to Haskell by ERDI Gergo http://gergo.erdi.hu/

module Main where

import System.ZMQ
import Control.Monad (forever, when)
import Data.ByteString.Char8 (pack)
import System.Random (randomRIO)
import Data.Function (fix)

main = withContext 1 $ \context -> do  
  -- This is where the weather server sits
  withSocket context Sub $ \frontend -> do
    connect frontend "tcp://192.168.55.210:5556"
    subscribe frontend ""
    
    -- This is our public endpoint for subscribers
    withSocket context Pub $ \backend -> do
      bind backend "tcp://10.1.1.0:8100"
      -- Subscribe on everything
      
      -- Shunt messages out to our own subscribers
      forever $ do
        fix $ \loop -> do
          message <- receive frontend []
          more <- moreToReceive frontend
          send backend message [SndMore | more]
          when more loop
