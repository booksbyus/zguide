{-# LANGUAGE OverloadedStrings #-}
-- |
-- Load balancing broker (p.96)
-- (Clients) [REQ] >-> (frontend) ROUTER (Proxy) ROUTER (backend) >-> [REQ] (Workers)
-- Clients and workers are shown here in-process
-- Compile with -threaded

module Main where

import System.ZMQ4.Monadic

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

    send sock [] "GO"
    msg <- receive sock
    liftIO $ printf "%s : %s\n" ident (unpack msg)


-- | Handle worker activity on backend
processBackend :: (Receiver r, Sender s) => [String] -> Int ->  Socket z r -> Socket z s -> [Event] -> ZMQ z ([String], Int)
processBackend  availableWorkers clientCount backend frontend evts
    -- A msg can be received without bloking
    | In `elem` evts = do 
        -- the msg comes from a worker: first frame is the worker id
        workerId <- unpack <$> receive backend

        empty <- unpack <$> receive backend
        when (empty /= "") $ error "The second frame should be empty"
    
        let workerQueue = availableWorkers ++ [workerId]
        
        -- the third frame is the msg "READY" from a  or a client reply id
        msg <- unpack <$> receive backend
        if msg == "READY"
            then 
                return (workerQueue, clientCount)
            else do
                empty' <- unpack <$> receive backend
                when (empty' /= "") $ error "The fourth frame should be an empty delimiter"
                -- the fifth frame is the client message
                reply <- receive backend
                -- send back an acknowledge msg to the client (msg is the clientId)
                send frontend [SendMore] (pack msg) 
                send frontend [SendMore] ""
                send frontend [] reply
                -- decrement clientCount to mark a job done
                return (workerQueue, clientCount - 1)

    | otherwise = return (availableWorkers, clientCount)



processFrontend :: (Receiver r, Sender s) => [String] -> Socket z r -> Socket z s -> [Event] -> ZMQ z [String]
processFrontend  availableWorkers frontend backend  evts 
    | In `elem` evts = do
            clientId <- receive frontend
            empty <- unpack <$> receive frontend
            when (empty /= "") $ error "The second frame should be empty"
            request <- receive frontend
            
            send backend [SendMore] (pack $ head availableWorkers) 
            send backend [SendMore] ""
            send backend [SendMore] clientId
            send backend [SendMore] ""
            send backend [] request
            return (tail availableWorkers)

    | otherwise = return availableWorkers


 
lruQueue :: Socket z Router -> Socket z Router -> ZMQ z ()
lruQueue backend frontend =
    -- start with an empty list of available workers
    loop [] nbrClients
    where
        loop availableWorkers clientCount  = do
            [evtsB, evtsF] <- poll (-1) [Sock backend [In] Nothing, Sock frontend [In] Nothing]
            -- (always) poll for workers activity
            (availableWorkers', clientCount') <- processBackend availableWorkers clientCount backend frontend evtsB
            when (clientCount' > 0) $ 
                --  Poll frontend only if we have available workers
                if not (null availableWorkers')
                    then do
                        availableWorkers'' <- processFrontend  availableWorkers' frontend backend evtsF 
                        loop availableWorkers'' clientCount'
                    else loop availableWorkers' clientCount'

main :: IO ()
main = 
    runZMQ $ do

        frontend <- socket Router
        bind frontend "inproc://clients"
        backend <- socket Router
        bind backend "inproc://workers"

        forM_ [1..nbrWorkers] $ \i -> async (workerThread i)
        forM_ [1..nbrClients] $ \i -> async (clientThread i)

        lruQueue backend frontend
        liftIO $ threadDelay $ 1 * 1000 * 1000
        