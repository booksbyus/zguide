-- |
-- Multithreaded relay in Haskell
-- 
-- Originally translated to Haskell by ERDI Gergo http://gergo.erdi.hu/

module Main where

import System.ZMQ3.Monadic (ZMQ, runZMQ, socket, Pair(..), bind, connect, receive, send, async, liftIO)

import Data.ByteString.Char8 (pack, unpack)

step1 :: ZMQ z ()
step1 = do
  -- Connect to step2 and tell it we're ready
  xmitter <- socket Pair
  connect xmitter "inproc://step2"
  send xmitter [] (pack "READY")

step2 :: ZMQ z ()
step2  = do
    receiver <- socket Pair
    -- Bind inproc socket before starting step1
    bind receiver "inproc://step2"
    async step1
    
    -- Wait for signal and pass it on
    msg <- receive receiver
    
    -- Connect to step3 and tell it we're ready
    xmitter <- socket Pair
    connect xmitter "inproc://step3"
    send xmitter [] msg
      
main :: IO ()
main = 
    runZMQ $ do
    -- Bind inproc socket before starting step2
    receiver <- socket Pair
    bind receiver "inproc://step3"
    async step2
    
    -- Wait for signal
    msg <- receive receiver
    liftIO $ putStrLn $ unwords ["Test successful!", unpack msg]
