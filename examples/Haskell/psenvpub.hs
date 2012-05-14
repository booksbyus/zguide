module Main where

import System.ZMQ
import Data.ByteString.Char8 (pack)
import Control.Concurrent (threadDelay)
import Control.Monad (forever)

main :: IO ()
main = withContext 1 $ \context -> do
    withSocket context Pub $ \publisher -> do
        bind publisher "tcp://*:5563"
        forever $ do
            send publisher (pack "A") [SndMore]
            send publisher (pack "We don't want to see this") []
            send publisher (pack "B") [SndMore]
            send publisher (pack "We would like to see this") []
            threadDelay $ 1 * 1000 * 1000