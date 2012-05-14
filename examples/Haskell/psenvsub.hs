module Main where
    
import System.ZMQ
import Data.ByteString.Char8 (unpack)
import Control.Monad (forever)

main :: IO ()
main = withContext 1 $ \context -> do
    withSocket context Sub $ \subscriber -> do
        connect subscriber "tcp://localhost:5563"
        subscribe subscriber "B"
        forever $ do
            addr <- receive subscriber []
            mre <- moreToReceive subscriber 
            contents <- if mre then fmap unpack $ receive subscriber []
                               else return ""
            putStrLn $ "[" ++ show addr ++ "] " ++ contents
                