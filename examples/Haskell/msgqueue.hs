-- |
-- Simple message queuing broker in Haskell
-- Same as request-reply broker but using QUEUE device
-- 
-- Orginally translated to Haskell by ERDI Gergo http://gergo.erdi.hu/

module Main where

import System.ZMQ3

main :: IO ()
main = 
	withContext $ \ctx ->
  		withSocket ctx Router $ \frontend ->
    	withSocket ctx Dealer $ \backend -> do
      
      	bind frontend "tcp://*:5559"
      	bind backend "tcp://*:5560"      
      	proxy frontend backend Nothing
