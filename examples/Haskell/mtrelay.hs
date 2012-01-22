-- |
-- Multithreaded relay in Haskell
-- 
-- Translated to Haskell by ERDI Gergo http://gergo.erdi.hu/

module Main where

import System.ZMQ
import Data.ByteString.Char8 (pack, unpack)
import Control.Concurrent (threadDelay, forkIO)

step1 :: Context -> IO ()
step1 context = do
  -- Connect to step2 and tell it we're ready
  withSocket context Pair $ \xmitter -> do
    connect xmitter "inproc://step2"
    send xmitter (pack "READY") []

step2 :: Context -> IO ()
step2 context = do
  withSocket context Pair $ \receiver -> do
    -- Bind inproc socket before starting step1
    bind receiver "inproc://step2"
    forkIO $ step1 context
    
    -- Wait for signal and pass it on
    msg <- receive receiver []
    
    -- Connect to step3 and tell it we're ready
    withSocket context Pair $ \xmitter -> do
      connect xmitter "inproc://step3"
      send xmitter msg []
      
main :: IO ()
main = withContext 1 $ \context -> do  
  -- Bind inproc socket before starting step2
  withSocket context Pair $ \receiver -> do
    bind receiver "inproc://step3"
    forkIO $ step2 context
    
    -- Wait for signal
    msg <- receive receiver []
    putStrLn $ unwords ["Test successful!", unpack msg]
