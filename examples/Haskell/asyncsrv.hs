-- |
-- Asynchronous client-to-server (DEALER to ROUTER) p.111
-- Compile with -threaded

module Main where

import System.ZMQ4.Monadic
import ZHelpers (setRandomIdentity)
import Control.Concurrent (threadDelay)
import Data.ByteString.Char8 (pack, unpack)
import Control.Monad (forever, forM_, replicateM_)
import System.Random (randomRIO)
import Text.Printf

clientTask :: String -> ZMQ z ()
clientTask ident = do
    client <- socket Dealer
    setRandomIdentity client
    connect client "tcp://localhost:5570"
    forM_ [1..] $ \i -> do -- (long enough) forever
        -- tick one per second, pulling in arriving messages
        forM_ [0..100] $ \_ -> 
            poll 10 -- timeout of 10 ms
                [Sock client [In] -- wait for incoming event 
                $ Just $ -- if it happens do
                    \_ -> receive client >>= liftIO . printf "Client %s has received back from worker its msg \"%s\"\n" ident . unpack ] 
              
        send client [] (pack $ unwords ["Client", ident, "sends request", show i])

            
serverTask :: ZMQ z ()
serverTask =  do
    frontend <- socket Router
    bind frontend "tcp://*:5570"
    backend <- socket Dealer
    bind backend "inproc://backend"
    
    replicateM_ 5 $ async serverWorker

    proxy frontend backend Nothing


serverWorker :: ZMQ z ()
serverWorker = do
    worker <- socket Dealer
    connect worker "inproc://backend"
    liftIO $ putStrLn "Worker Started"        
    forever $ -- receive both ident and msg and send back the msg to the ident client.
        receive worker >>= \ident -> receive worker >>= \msg -> sendback worker msg ident

    where
         -- send back to client 0 to 4 times max
        sendback worker msg ident  = do
            resentNb <- liftIO $ randomRIO (0, 4)
            timeoutMsec <- liftIO $ randomRIO (1, 1000) 
            forM_ [0::Int ..resentNb] $ \_ -> do
                liftIO $ threadDelay $ timeoutMsec * 1000
                send worker [SendMore] ident
                send worker [] msg

main :: IO ()
main = 
    runZMQ $ do
        async $ clientTask "A"
        async $ clientTask "B"
        async $ clientTask "C"
        async serverTask
        
        liftIO $ threadDelay $ 5 * 1000 * 1000