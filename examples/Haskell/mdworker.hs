import MDWorkerAPI

import ZHelpers

import System.Environment (getArgs)
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import Control.Monad (forever, mapM_, when)
import Data.ByteString.Char8 (unpack, empty, ByteString(..))

main :: IO ()
main = do
    args <- getArgs
    when (length args /= 1) $
        error "usage: mdworker <isVerbose(True|False)>"
    let isVerbose = read (args !! 0) :: Bool

    hSetBuffering stdout NoBuffering

    withMDWorker "tcp://localhost:5555" "echo" isVerbose $ \session ->
        doEcho session [empty]
      where doEcho session reply = do
                request <- mdwkrExchange session reply
                doEcho (fst request) (snd request)
