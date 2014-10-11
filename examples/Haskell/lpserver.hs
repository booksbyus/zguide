{--
Lazy Pirate server in Haskell    
--}
module Main where

import System.ZMQ4.Monadic
import System.Random (randomRIO)
import System.Exit (exitSuccess)
import Control.Monad (forever, when)
import Control.Concurrent (threadDelay)
import Data.ByteString.Char8 (pack, unpack)


main :: IO ()
main =
    runZMQ $ do
        server <- socket Rep
        bind server "tcp://*:5555"

        sendClient 0 server

sendClient :: Int -> Socket z Rep -> ZMQ z ()
sendClient cycles server = do
    req <- receive server

    chance <- liftIO $ randomRIO (0::Int, 3)
    when (cycles > 3 && chance == 0) $ do
        liftIO crash

    chance' <- liftIO $ randomRIO (0::Int, 3)
    when (cycles > 3 && chance' == 0) $ do
        liftIO overload

    liftIO $ putStrLn $ "I: normal request " ++ (unpack req)
    liftIO $ threadDelay $ 1 * 1000 * 1000
    send server [] req

    sendClient (cycles+1) server

crash = do
    putStrLn "I: Simulating a crash"
    exitSuccess
overload = do 
    putStrLn "I: Simulating CPU overload"
    threadDelay $ 2 * 1000 * 1000
