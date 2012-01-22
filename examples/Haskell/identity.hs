module Main where

import System.ZMQ
import ZHelpers
import Data.ByteString.Char8

main :: IO ()
main = withContext 1 $ \context -> do
    withSocket context XRep $ \sink -> do
        bind sink "inproc://example"
        withSocket context Req $ \anonymous -> do
            connect anonymous "inproc://example"
            send anonymous (pack "ROUTER uses a generated UUID") []
            dump_sock sink
            
        withSocket context Req $ \identified -> do
            setOption identified (Identity "Hello")
            connect identified "inproc://example"
            send identified (pack "XRep socket uses REQ's socket identity") []
            dump_sock sink