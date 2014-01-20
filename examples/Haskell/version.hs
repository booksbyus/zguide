module Main where

import System.ZMQ4 (version)
import Text.Printf (printf)

main :: IO ()
main = do
    (major, minor, patch) <- version
    printf "Current 0MQ version is %d.%d.%d" major minor patch
