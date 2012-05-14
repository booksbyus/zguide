module Main where

import System.ZMQ
import Control.Monad (forever)
    
main :: IO ()
main = withContext 1 $ \context -> do
    withSocket context XRep $ \frontend -> do
        bind frontend "tcp://*:5559"
        withSocket context XReq $ \backend -> do
            bind backend "tcp://*5560"
            forever $ loopFunction frontend backend (-1)

loopFunction :: Socket a -> Socket b -> Timeout -> IO ()
loopFunction front back timeout = do
    [S _front' status1, S _back' status2] <- poll [S front In, S back In] timeout
    process front back status1
    process back front status2

process :: Socket a -> Socket b -> PollEvent -> IO ()
process sock_recv sock_to_send None = return ()
process sock_recv sock_to_send In = do
    msg <- receive sock_recv []
    more <- moreToReceive sock_recv
    if more 
        then do
            send sock_to_send msg [SndMore]
            process sock_recv sock_to_send In
        else send sock_to_send msg []
