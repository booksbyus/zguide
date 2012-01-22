module Main where
    
import System.ZMQ
import Data.ByteString.Char8 (unpack, pack)
import Control.Concurrent (threadDelay)

main :: IO ()
main = withContext 1 $ \context -> do
    withSocket context Sub $ \subscriber -> do
        connect subscriber "tcp://localhost:5561"
        subscribe subscriber ""
        threadDelay $ 1 * 1000 * 1000
        withSocket context Req $ \syncclient -> do
            connect syncclient "tcp://localhost:5562"
            send syncclient (pack "") []
            receive syncclient []
            nbr <- counts 0 syncclient
            putStrLn $ "Received " ++ show nbr ++ " updates"

counts :: Int -> Socket a -> IO Int
counts val sock = do
    msg <- receive sock []
    if unpack msg == "END"
        then return val 
        else counts (val + 1) sock