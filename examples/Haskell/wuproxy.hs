{-# LANGUAGE OverloadedStrings #-}
-- |
-- Weather proxy device in Haskell
-- 
-- Originally translated to Haskell by ERDI Gergo http://gergo.erdi.hu/

module Main where

import System.ZMQ3.Monadic (runZMQ, socket, connect, bind, receive, send, moreToReceive, subscribe, Sub(..), Pub(..), Flag(SendMore), liftIO)
import Control.Monad (forever)

main :: IO ()
main =
    runZMQ $ do
        frontend <- socket Sub
        connect frontend "tcp://localhost:5556"
        -- Subscribe on everything
        subscribe frontend ""
    
        -- This is our public endpoint for subscribers
        backend <- socket Pub
        bind backend "tcp://*:8100"
      
        -- Shunt messages out to our own subscribers
        forever $ shunt frontend backend

    where
        -- this implementation is an example
        -- it works but it is quite slow
        -- Use the proxy function in real situation
        shunt from to = do
                    msg <- receive from
                    more <- moreToReceive from
                    if more
                    then send to [SendMore] msg
                    else send to [] msg


