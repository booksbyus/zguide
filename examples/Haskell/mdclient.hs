import MDClientAPI

import ZHelpers

import System.Environment
import Control.Monad (forM_, when)
import Data.ByteString.Char8 (unpack, pack)

main :: IO ()
main = do
    args <- getArgs
    when (length args /= 1) $
        error "usage: mdclient <is_verbose(True|False)>"
    let isVerbose = read (args !! 0) :: Bool

    withMDCli "tcp://localhost:5555" isVerbose $ \api ->
        forM_ [0..10000] $ \i -> do
            mdSend api "echo" [pack "Hello world"]
