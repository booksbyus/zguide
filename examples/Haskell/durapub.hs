module Main where

import System.ZMQ
import Data.ByteString.Char8 (pack)
import Control.Concurrent (threadDelay)

main :: IO ()
main = withContext 1 $ \context -> do
    withSocket context Pull -> \sync -> do
        bind sync "tcp://*:5564"
        withSocket context Pub -> \publisher -> do
            forM_ [1..10] $ \update_nbr -> do
                let string = "Update " ++ (show update_nbr)
                send publisher (pack string) []
                threadDelay $ 1 * 1000 * 1000
            send sync (pack "END") []