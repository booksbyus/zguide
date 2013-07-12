{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.ZMQ3.Monadic

import Control.Concurrent (threadDelay)
import Data.ByteString.Char8 (pack, unpack)
import Control.Monad (forM_, forever, when)
import Control.Applicative ((<$>))
import Text.Printf

nbrClients :: Int
nbrClients = 10

nbrWorkers :: Int
nbrWorkers = 3

workerThread :: Show a => a -> ZMQ z ()
workerThread i = do
    sock <- socket Req
    let ident = "Worker-" ++ show i
    setIdentity (restrict $ pack ident) sock
    connect sock "inproc://workers"
    send sock [] "READY" 

    forever $ do
        address <- receive sock
        receive sock -- empty frame
        receive sock >>= liftIO . printf "%s : %s\n" ident . unpack
         
        send sock [SendMore] address 
        send sock [SendMore] ""
        send sock [] "OK"
    

clientThread :: Show a => a -> ZMQ z ()
clientThread i = do
    sock <- socket Req
    let ident = "Client-" ++ show i
    setIdentity (restrict $ pack ident) sock
    connect sock "inproc://clients"

    send sock [] "HELLO" 
    receive sock >>= liftIO . printf "%s : %s\n" ident . unpack


processBackend :: (Receiver r, Sender s) => Int -> [String] -> Int -> Socket z r -> Socket z s -> [Event] -> ZMQ z (Int, [String], Int, Bool)
processBackend avail_workers workers_list client_nbr backend frontend evts 
    | In `elem` evts = do
        -- first frame is the worker id
        worker_id <- unpack <$> receive backend
        when (avail_workers >= nbrWorkers) $ error ""

        -- second frame is an empty frame
        empty <- unpack <$> receive backend
        when (empty /= "") $ error ""
        
        let avail' = avail_workers + 1
        let work_list = workers_list ++ [worker_id]
        
        -- the third frame is "READY" or a client reply id
        client_id <- unpack <$> receive backend
        if client_id == "READY"
        then do
            return (avail', work_list, client_nbr, True)
        else do
            -- the fourth frame is the empty delimiter
            empty' <- unpack <$> receive backend
            when (empty' /= "") $ error ""
            -- the fifth frame is the client message
            reply <- receive backend
        
            send frontend [SendMore] (pack client_id) 
            send frontend [SendMore] ""
            send frontend [] reply
            return (avail', work_list, client_nbr - 1, True)

    | otherwise = return (avail_workers, workers_list, client_nbr, False)



processFrontend :: (Receiver r, Sender s) => Int -> [String] -> Socket z r -> Socket z s -> [Event] -> ZMQ z (Int, [String])
processFrontend avail_workers workers_list frontend backend  evts 
    | In `elem` evts = do
            client_id <- receive frontend
            empty <- unpack <$> receive frontend
            when (empty /= "") $ error ""
            request <- receive frontend
            
            send backend [SendMore] (pack $ head workers_list) 
            send backend [SendMore] ""
            send backend [SendMore] client_id
            send backend [SendMore] ""
            send backend [] request
            return (avail_workers - 1, tail workers_list)

    | otherwise = return (avail_workers, workers_list)


 

main :: IO ()
main = do
        
    runZMQ $ do

        frontend <- socket Router
        bind frontend "inproc://clients"
        backend <- socket Router
        bind backend "inproc://workers"

        forM_ [1..nbrWorkers] $ \i -> async (workerThread i)
        forM_ [1..nbrClients] $ \i -> async (clientThread i)

        lruQueue backend frontend
        
        liftIO $ threadDelay $ 1 * 1000 * 1000
    
    where

        lruQueue = loop 0 [] nbrClients where
            loop avail_workers workers_list client_nbr backend frontend  = do
                
                [evtsB, evtsF] <- poll (-1) [Sock backend [In] Nothing, Sock frontend [In] Nothing]
                
                (avail_workers', workers_list', client_nbr', processed) <- processBackend avail_workers workers_list client_nbr backend frontend evtsB
                when (client_nbr > 0) $ do
                    if processed
                    then do 
                        (avail_workers'', workers_list'') <- processFrontend avail_workers' workers_list' frontend backend evtsF 
                        loop avail_workers'' workers_list'' client_nbr' backend frontend
                    else loop avail_workers' workers_list' client_nbr' backend frontend