{-# LANGUAGE OverloadedStrings #-}
-- |
-- Multiple socket poller in Haskell
-- This version uses poll
--
-- Originally translated to Haskell by Sebastian Nowicki <sebnow@gmail.com>

module Main where

import Control.Monad (forever, when)
import Data.ByteString.Char8 (unpack)
import System.ZMQ3
import Control.Applicative ((<$>))

main :: IO ()
main = 
    withContext $ \ctx ->
        withSocket ctx Pull $ \receiver ->
        withSocket ctx Sub $ \subscriber -> do
            connect receiver "tcp://localhost:5557"
            setIdentity (restrict "vent receiver") receiver
            connect subscriber "tcp://localhost:5556"
            subscribe subscriber "10001"
            forever $ do
                poll (-1) [Sock receiver [In] (Just $ processEvts receiver), Sock subscriber [In] (Just $ processEvts subscriber)]

processEvts :: (Receiver a) => Socket a -> [Event] -> IO ()
processEvts sock evts = do
    when (In `elem` evts) $ do
        msg <- unpack <$> receive sock
        ident <- unpack <$> identity sock
        putStrLn $ unwords $ ["Processing ", ident, msg]

