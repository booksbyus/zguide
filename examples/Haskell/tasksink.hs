-- |
-- Task sink
-- Binds PULL socket to tcp://localhost:5558
-- Collects results from workers via that socket
-- 
-- Translated to Haskell by ERDI Gergo http://gergo.erdi.hu/

module Main where

import System.ZMQ3.Monadic(runZMQ, socket, bind, receive, Pull(..), liftIO)
import Control.Monad (forM_)
import System.IO (hSetBuffering, stdout, BufferMode(..))
import Data.Time.Clock (getCurrentTime, diffUTCTime)

main :: IO ()
main = do
    runZMQ $ do
        receiver <- socket Pull
        bind receiver "tcp://*:5558"
        -- Wait for start of batch
        receive receiver
        -- Start our clock now
        startTime <- liftIO $ getCurrentTime
        liftIO $ hSetBuffering stdout NoBuffering

        -- Process 100 confirmations    
        forM_ [1..100] $ \i -> do
            receive receiver
            liftIO $ putStr $ if i `mod` 10 == 0 then ":" else "."
          
        endTime <- liftIO $ getCurrentTime
        let elapsedTime = diffUTCTime endTime startTime
            elapsedMsec = elapsedTime * 1000
        liftIO $ putStrLn $ unwords ["Total elapsed time:", show elapsedMsec, "msecs"]
