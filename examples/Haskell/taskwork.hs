-- |
-- Task worker
-- Connects PULL socket to tcp://localhost:5557
-- Collects workloads from ventilator via that socket
-- Connects PUSH socket to tcp://localhost:5558
-- Sends results to sink via that socket
-- 
-- Translated to Haskell by ERDI Gergo http://gergo.erdi.hu/

module Main where

import System.ZMQ
import Control.Monad (forever)
import Data.ByteString.Char8 (unpack, empty)
import System.Random (randomRIO)
import Control.Applicative ((<$>))
import System.IO (hSetBuffering, stdout, BufferMode(..))
import Control.Concurrent (threadDelay)


main :: IO ()
main = withContext 1 $ \context -> do  
  withSocket context Pull $ \receiver -> do
    connect receiver "tcp://localhost:5557"
    withSocket context Push $ \sender -> do    
      connect sender "tcp://localhost:5558"
      
      hSetBuffering stdout NoBuffering
      forever $ do
        message <- unpack <$> receive receiver []
        -- Simple progress indicator for the viewer
        putStr $ message ++ "."

        -- Do the "work"
        threadDelay (read message * 1000)
       
        -- Send results to sink
        send sender empty []
