{-# LANGUAGE OverloadedStrings #-}
-- |
-- Hello World client in Haskell
-- Binds REQ socket to tcp://localhost:5559
-- Sends "Hello" to server, expects "World" back
-- 
-- Originally translated to Haskell by ERDI Gergo http://gergo.erdi.hu/

module Main where

import System.ZMQ3.Monadic (runZMQ, socket, connect, send, receive, Req(..), liftIO)
import Control.Monad (forM_)
import Data.ByteString.Char8 (unpack)
import Text.Printf

main :: IO ()
main = 
    runZMQ $ do
        requester <- socket Req
        connect requester "tcp://localhost:5559"
        forM_ [1..10] $ \i -> do
            send requester [] "Hello"  
            msg <- receive requester
            liftIO $ printf "Received reply %d %s\n" (i ::Int) (unpack msg)

