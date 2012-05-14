module Main where

import System.ZMQ (version)
import Text.Printf (printf)

main :: IO ()
main = version >>= putStrLn . fmt where
    fmt (major, minor, patch) = printf "Current 0MQ version is %d.%d.%d" major minor patch