-- |
-- Simple message queuing broker (p.53)
-- Same as request-reply broker but using QUEUE device
-- 
-- Use it with `rrclient.hs` and `rrworker.hs`

module Main where

import System.ZMQ4

main :: IO ()
main = 
	withContext $ \ctx ->
  		withSocket ctx Router $ \frontend ->
    	withSocket ctx Dealer $ \backend -> do
      
      	bind frontend "tcp://*:5559"
      	bind backend "tcp://*:5560"      
      	proxy frontend backend Nothing
