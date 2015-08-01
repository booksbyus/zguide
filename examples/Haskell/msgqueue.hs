--  Simple message queuing broker
--  Same as request-reply broker but using shared queue proxy

module Main where

import System.ZMQ4.Monadic

main :: IO ()
main = runZMQ $ do
    -- Socket facing clients
    frontend <- socket Router
    bind frontend "tcp://*:5559"

    backend <- socket Dealer
    bind backend "tcp://*:5560"

    -- Start the proxy
    proxy frontend backend Nothing

