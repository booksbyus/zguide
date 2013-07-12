module Main where

import System.ZMQ3.Monadic
import Control.Monad (forever)    

main :: IO ()
main = 
    runZMQ $ do 
        frontend <- socket Router
        bind frontend "tcp://*:5559"
        backend <- socket Dealer
        bind backend "tcp://*:5560"
        proxy frontend backend Nothing