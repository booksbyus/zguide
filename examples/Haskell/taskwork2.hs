{-# LANGUAGE OverloadedStrings #-}
-- |
-- Task worker - design 2 (p.58)
-- Add pub-sub flow to receive and respond to kill signal

module Main where

import System.ZMQ4
import Data.ByteString.Char8 (unpack, empty)
import System.IO (hSetBuffering, stdout, BufferMode(..))
import Control.Concurrent (threadDelay)
import Control.Applicative ((<$>))
import Control.Monad (when, unless)

main :: IO ()
main =
    withContext $ \ctx -> 
        withSocket ctx Pull $ \receiver ->
        withSocket ctx Push $ \sender -> 
        withSocket ctx Sub $ \controller -> do
            connect receiver "tcp://localhost:5557"
            connect sender "tcp://localhost:5558"
            connect controller "tcp://localhost:5559"
            subscribe controller ""
            hSetBuffering stdout NoBuffering
            pollContinuously receiver sender controller

    where

    pollContinuously ::  (Receiver r, Sender s) => Socket r -> Socket s -> Socket c -> IO ()
    pollContinuously sock_recv sock_to_send ctr  = do

        [a, b] <- poll (-1) [Sock sock_recv [In] Nothing, Sock ctr [In] Nothing] 

        when (In `elem` a) $ do
            msg <- unpack <$> receive sock_recv
            -- Simple progress indicator for the viewer
            putStr $ msg ++ "."
            -- Do the "work"
            threadDelay (read msg * 1000)
            -- Send results to sink
            send sock_to_send [] empty

        unless (In `elem` b) $
             pollContinuously sock_recv sock_to_send ctr
