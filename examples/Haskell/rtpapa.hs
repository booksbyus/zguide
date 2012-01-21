module Main where

import System.ZMQ
import ZHelpers

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Data.ByteString.Char8 (pack, unpack)
import Control.Monad (forM_)

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
            
            dump_sock worker
            send worker (pack "This is the reply") []
            
            dump_sock client