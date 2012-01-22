module Main where

import System.ZMQ
import ZHelpers

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Data.ByteString.Char8 (pack, unpack)
import Control.Monad (forM_, forever)

nbr_clients :: Int
nbr_clients = 10

nbr_workers :: Int
nbr_workers = 3

worker_thread url ctx i = withSocket ctx Req $ \socket -> do
    let identity = "Worker-" ++ (show i)
    setOption socket (Identity identity)
    connect socket url
    send socket (pack "READY") []
    
    forever $ do
        address <- receive socket []
        empty <- receive socket []
        request <- fmap unpack $ receive socket []
    
        putStrLn $ identity ++ ": " ++ request
        
        send socket address [SndMore]
        send socket (pack "") [SndMore]
        send socket (pack "OK") []
    
    
client_thread url ctx i = withSocket ctx Req $ \socket -> do
    let identity = "Client-" ++ (show i)
    setOption socket (Identity identity)
    connect socket url
    
    send socket (pack "HELLO") []
    reply <- fmap unpack $ receive socket []
    
    putStrLn $ identity ++ ": " ++ reply

backend_func None a b c _ _ = return (a, b, c)
backend_func In avail_workers workers_list client_nbr backend frontend = do 
    worker_addr <- receive backend []
    if (avail_workers < nbr_workers) then return () else error ""

    empty <- fmap unpack $ receive backend []
    if (empty == "") then return () else error ""
    
    let avail' = avail_workers + 1
    let work_list = workers_list ++ [show avail']
    
    client_addr <- fmap unpack $ receive backend []
    if (client_addr == "READY") then return (avail', work_list, client_nbr) else do
        empty' <- fmap unpack $ receive backend []
        if (empty' == "") then return () else error ""
        
        reply <- receive backend []
        
        send frontend (pack client_addr) [SndMore]
        send frontend (pack "") [SndMore]
        send frontend reply []
        return (avail', work_list, client_nbr - 1)

frontend_func None a b _ _ _ = return (a, b)
frontend_func In 0 b _ _ _ = return (0, b)
frontend_func In avail_workers workers_list client_nbr frontend backend = do
    client_addr <- receive frontend []
    empty <- fmap unpack $ receive frontend []
    if (empty == "") then return () else error ""
    request <- receive frontend []
    
    let worker_id = head workers_list 
    
    send backend (pack $ show worker_id) [SndMore]
    send backend (pack "") [SndMore]
    send backend client_addr [SndMore]
    send backend (pack "") [SndMore]
    send backend request []
    
    return (avail_workers - 1, tail workers_list)
    


lru_queue_func avail_workers workers_list client_nbr backend frontend = do
    [S backend' res1, S frontend' res2] <- poll [S backend In, S frontend In] (-1)
    (avail_workers', workers_list', client_nbr') <- backend_func res1 avail_workers workers_list client_nbr backend frontend
    if (client_nbr' == 0) then return () else do
        (avail_workers'', workers_list'') <- frontend_func res2 avail_workers' workers_list' client_nbr' frontend backend
        lru_queue_func avail_workers'' workers_list'' client_nbr' backend frontend
     

main :: IO ()
main = do
    let url_worker = "inproc://workers"
        url_client = "inproc://clients"
        client_nbr = nbr_clients
        
    withContext 1 $ \context -> do
        withSocket context XRep $ \frontend -> do
            bind frontend url_client
            withSocket context XRep $ \backend -> do
                bind backend url_worker
                forM_ [1..nbr_workers] $ \i -> forkIO (worker_thread url_worker context i)
                forM_ [1..nbr_clients] $ \i -> forkIO (client_thread url_client context i)
                lru_queue_func 0 [] client_nbr backend frontend
                threadDelay $ 1 * 1000 * 1000
                