module Main where

import System.ZMQ
import Data.ByteString.Char8 (pack)
import Control.Monad (forM_) 
    
subscribers_expected :: Int
subscribers_expected = 2
    
main :: IO ()
main = withContext 1 $ \context -> do
    withSocket context Pub $ \publisher -> do
        bind publisher "tcp://*:5561"
        withSocket context Rep $ \syncservice -> do
            bind syncservice "tcp://*:5562"
            getSubs 0 syncservice
            forM_ [1..1000000] $ \_ -> send publisher (pack "Rhubarb") []
            send publisher (pack "END") []
            
            
getSubs num sock | num >= subscribers_expected = return ()
                 | otherwise = do
                     msg <- receive sock []
                     send sock (pack "") []
                     getSubs (num + 1) sock