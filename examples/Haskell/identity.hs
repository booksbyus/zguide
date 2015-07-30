{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.ZMQ4.Monadic
import ZHelpers (dumpSock)

main :: IO ()
main =
    runZMQ $ do
        
        sink <- socket Router
        bind sink "inproc://example"

        anonymous <- socket Req
        connect anonymous "inproc://example"
        send anonymous [] "ROUTER uses a generated 5 byte identity" 

        dumpSock sink

        identified <- socket Req
        setIdentity (restrict "PEER2") identified
        connect identified "inproc://example"
        send identified [] "ROUTER socket uses REQ's socket identity" 
               
        dumpSock sink 
