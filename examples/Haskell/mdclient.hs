import MDClientAPI

import ZHelpers

import Control.Monad (forM_)
import Data.ByteString.Char8 (unpack, pack)

main :: IO ()
main = 
    withMDCli "tcp://localhost:5555" False $ \api ->
        forM_ [0..] $ \i -> do
            mdSend api "echo" [pack "Hello world"]
