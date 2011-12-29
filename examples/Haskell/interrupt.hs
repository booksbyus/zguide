module Main where

import System.Posix.Signals
import Control.Concurrent
import Control.Concurrent.MVar

import System.ZMQ

handler :: MVar Int -> IO ()
handler s_interrupted = modifyMVar_ s_interrupted (return . (+1))

main :: IO ()
main = withContext 1 $ \context -> do
    withSocket context Rep $ \socket -> do
      bind socket "tcp://*:5555"
      s_interrupted <- newMVar 0
      installHandler sigINT (Catch $ handler s_interrupted) Nothing
      installHandler sigTERM (Catch $ handler s_interrupted) Nothing
      recv_function s_interrupted socket
      
recv_function mi sock = do
    receive sock []
    withMVar mi (\val -> if (val > 0) then (putStrLn "W: Interrupt Received. Killing Server") else (recv_function mi sock))