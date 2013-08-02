-- |
-- Hello World client in Haskell
-- Binds REQ socket to tcp://localhost:5555
-- Sends "Hello" to server, expects "World" back
-- 
-- Originally translated to Haskell by ERDI Gergo http://gergo.erdi.hu/

module Main where

import System.ZMQ3.Monadic
import Control.Monad (forM_)
import Data.ByteString.Char8 (pack, unpack)

main :: IO ()
main = 
    runZMQ $ do
        liftIO $ putStrLn "Connecting to Hello World server..."  
        reqSocket <- socket Req
        connect reqSocket "tcp://localhost:5555"
        forM_ [1..10] $ \i -> do
            liftIO $ putStrLn $ unwords ["Sending request", show i]
            send reqSocket [] (pack "Hello")
            reply <- receive reqSocket
            liftIO $ putStrLn $ unwords ["Received reply:", unpack reply]    

