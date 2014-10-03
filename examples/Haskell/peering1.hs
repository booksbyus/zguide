{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad         (forever, when)

import qualified Data.ByteString.Char8 as C
import           Data.Semigroup        ((<>))

import           System.Environment
import           System.Exit
import           System.Random

import           System.ZMQ4.Monadic

connectPeer :: Socket z t -> String -> String -> ZMQ z ()
connectPeer sock name peer = do
    connect sock (connectString peer name)
    liftIO . putStrLn $ "Connecting to peer: " ++ connectString peer name

connectString :: String -> String -> String
connectString peer name = "ipc://" ++ peer ++ "-" ++ name ++ ".ipc"

main :: IO ()
main = do
    args <- getArgs
    when (length args < 2) $ do
        putStrLn "Usage: peering1 <me> <you> [<you> ...]"
        exitFailure

    let self:peers = args
    putStrLn $ "Preparing broker at " ++ self

    runZMQ $ do
        -- Bind state backend to endpoint
        stateBack <- socket Pub
        bind stateBack (connectString self "state")

        -- Connect state frontend to peers
        stateFront <- socket Sub
        subscribe stateFront ""
        mapM_ (connectPeer stateFront "state") peers

        -- Send status, collect status
        forever $ do
            let pollItem = Sock stateFront [In] (Just pollEvent)
                pollEvent _ = do
                    peerName:available:_ <- receiveMulti stateFront
                    liftIO . C.putStrLn $
                        peerName <> " " <> available <> " workers free"
            pollEvents <- poll oneSec [pollItem]
            when (pollEvents == [[]]) $ do
                r <- liftIO $ randomRIO (0, 9)
                sendMulti stateBack [C.pack self, C.pack (show (r :: Int))]
  where
    oneSec = 1000
