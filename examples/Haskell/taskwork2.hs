module Main where

import System.ZMQ
import Control.Monad (forever)
import Data.ByteString.Char8 (unpack, empty)
import System.IO (hSetBuffering, stdout, BufferMode(..))
import Control.Concurrent (threadDelay)

main :: IO ()
main = withContext 1 $ \context -> do  
  withSocket context Pull $ \receiver -> do
    connect receiver "tcp://localhost:5557"
    withSocket context Push $ \sender -> do    
      connect sender "tcp://localhost:5558"
      withSocket context Sub $ \controller -> do
          connect controller "tcp://localhost:5559"
          subscribe controller ""
          hSetBuffering stdout NoBuffering
          pollContinuously receiver controller sender


pollContinuously :: Socket a -> Socket b -> Socket c -> IO ()
pollContinuously rec cont sends = do
    [S rec' e1', S cont' e2'] <- poll [S rec In, S cont In] (-1)
    case e2' of
        In -> return ()
        None -> do
            msg <- receive rec []
            let sleep = (read $ unpack msg) :: Int
            threadDelay $ sleep * 1000 * 1000
            send sends msg []
            print "."
            pollContinuously rec cont sends