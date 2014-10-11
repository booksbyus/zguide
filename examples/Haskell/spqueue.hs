{--
Simple Pirate queue in Haskell
--}
module Main where

import System.ZMQ4.Monadic

import Control.Concurrent (threadDelay)
import Control.Applicative ((<$>))
import Control.Monad (when)
import Data.ByteString.Char8 (pack, unpack, empty)
import Data.List (intercalate)


type SockID = String

workerReady = "\001"

main :: IO ()
main = 
    runZMQ $ do
        frontend <- socket Router
        bind frontend "tcp://*:5555"
        backend <- socket Router
        bind backend "tcp://*:5556"

        pollPeers frontend backend []

pollPeers :: Socket z Router -> Socket z Router -> [SockID] -> ZMQ z ()
pollPeers frontend backend workers = do
    let toPoll = getPollList workers
    evts <- poll 0 toPoll

    workers' <- getBackend backend frontend evts workers
    workers'' <- getFrontend frontend backend evts workers' 

    pollPeers frontend backend workers''

  where getPollList [] = [Sock backend [In] Nothing]
        getPollList _  = [Sock backend [In] Nothing, Sock frontend [In] Nothing]

        getBackend :: Socket z Router -> Socket z Router -> 
                      [[Event]] -> [SockID] -> ZMQ z ([SockID])
        getBackend backend frontend evts workers = 
                if (In `elem` (evts !! 0))
                then do
                    wkrID <- receive backend
                    id <- (receive backend >> receive backend)
                    msg <- (receive backend >> receive backend)

                    when ((unpack msg) /= workerReady) $ do
                        liftIO $ putStrLn $ "I: sending backend - " ++ (unpack msg)
                        send frontend [SendMore] id
                        send frontend [SendMore] empty
                        send frontend [] msg
                    return $ (unpack wkrID):workers
                else return workers

        getFrontend :: Socket z Router -> Socket z Router ->
                       [[Event]] -> [SockID] -> ZMQ z [SockID]
        getFrontend frontend backend evts workers =
                 if (length evts > 1 && In `elem` (evts !! 1))
                 then do
                     id <- receive frontend
                     msg <- (receive frontend >> receive frontend)
                     liftIO $ putStrLn $ "I: msg on frontend - " ++ (unpack msg)
                     let wkrID = head workers
                     send backend [SendMore] (pack wkrID)
                     send backend [SendMore] empty
                     send backend [SendMore] id
                     send backend [SendMore] empty
                     send backend [] msg
                     return $ tail workers
                 else return workers
