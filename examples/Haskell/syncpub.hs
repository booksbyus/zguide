module Main where

import System.ZMQ
import Data.ByteString.Char8 (pack)
import Control.Monad (replicateM_) 
    
subscribersExpected :: Int
subscribersExpected = 2
    
main :: IO ()
main = withContext 1 $ \context -> do
    withSocket context Pub $ \publisher -> do
        bind publisher "tcp://*:5561"
        withSocket context Rep $ \syncservice -> do
            bind syncservice "tcp://*:5562"
            getSubs 0 syncservice
            replicateM_ 1000000 $ send publisher (pack "Rhubarb") []
            send publisher (pack "END") []
            
getSubs :: Int -> Socket a -> IO ()        
getSubs num sock | num >= subscribersExpected = return ()
                 | otherwise = do
                     msg <- receive sock []
                     send sock (pack "") []
                     getSubs (num + 1) sock