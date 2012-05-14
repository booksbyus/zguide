module Main where

import System.ZMQ 
import Data.ByteString.Char8 (unpack, pack)
import Control.Monad (unless)

main :: IO ()
main = withContext 1 $ \context -> do
    withSocket context Sub $ \subscriber -> do
        setOption subscriber (Identity "Hello")
        subscribe subscriber ""
        connect subscriber "tcp://localhost:5565"
        withSocket context Push $ \sync -> do
            connect sync "tcp://localhost:5564"
            send sync (pack "") []
            getUpdates subscriber
    
getUpdates :: Socket a -> IO ()        
getUpdates sock = do
    msg <- fmap unpack $ receive sock []
    putStrLn msg
    unless (msg == "END") (getUpdates sock)