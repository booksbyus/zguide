module Main where

import System.ZMQ
import ZHelpers (dumpSock)

import Control.Concurrent (threadDelay)
import Data.ByteString.Char8 (pack)

main :: IO ()
main = withContext 1 $ \context -> do
    withSocket context XRep $ \client -> do
        bind client "ipc://routing.ipc"
        withSocket context Rep $ \worker -> do
            setOption worker (Identity "A")
            connect worker "ipc://routing.ipc"
            
            threadDelay $ 2 * 1000 * 1000
            send client (pack "A") [SndMore]
            send client (pack "address 3") [SndMore]
            send client (pack "address 2") [SndMore]
            send client (pack "address 1") [SndMore]
            send client (pack "") [SndMore]
            send client (pack "This is the workload") []

            dumpSock worker
            send worker (pack "This is the reply") []
            
            dumpSock client