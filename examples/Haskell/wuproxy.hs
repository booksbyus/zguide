--  Weather proxy device

module Main where

import System.ZMQ4.Monadic

main :: IO ()
main = runZMQ $ do
    -- This is where the weather service sits
    frontend <- socket XSub
    connect frontend "tcp://192.168.55.210:5556"

    -- This is our public endpoint for subscribers
    backend <- socket XPub
    bind backend "tcp://10.1.1.0:8100"

    -- Run the proxy until the user interrupts us
    proxy frontend backend Nothing
