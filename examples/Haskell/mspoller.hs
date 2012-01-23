-- |
-- Multiple socket poller in Haskell
-- This version uses poll
--
-- Translated to Haskell by Sebastian Nowicki <sebnow@gmail.com>

module Main where

import Control.Monad (forever)
import Data.ByteString.Char8 (ByteString)
import System.ZMQ

main :: IO ()
main = withContext 1 $ \context -> do
    withSocket context Pull $ \receiver -> do
        connect receiver "tcp://localhost:5557"
        withSocket context Sub $ \subscriber -> do
            connect subscriber "tcp://localhost:5556"
            subscribe subscriber "10001"
            forever $ do
                putStrLn "Processing"
                poll [S receiver In, S subscriber In] (-1) >>= mapM_ (\(S s _) -> handleSocket s)

handleSocket :: Socket a -> IO ()
handleSocket socket = do
    msg <- receive socket []
    processMessage msg
    return ()

processMessage :: (Monad m) => ByteString -> m ()
processMessage _ = return ()

