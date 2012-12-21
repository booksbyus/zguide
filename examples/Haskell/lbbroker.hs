module Main where

import System.ZMQ

import Control.Concurrent (forkIO, threadDelay)
import Data.ByteString.Char8 (pack, unpack)
import Control.Monad (forM_, forever, when)

nbrClients :: Int
nbrClients = 10

nbrWorkers :: Int
nbrWorkers = 3

workerThread :: Show a => String -> Context -> a -> IO ()
workerThread url ctx i = withSocket ctx Req $ \socket -> do
    let identity = "Worker-" ++ show i
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
    

clientThread :: Show a => String -> Context -> a -> IO ()
clientThread url ctx i = withSocket ctx Req $ \socket -> do
    let identity = "Client-" ++ show i
    setOption socket (Identity identity)
    connect socket url
    
    send socket (pack "HELLO") []
    reply <- fmap unpack $ receive socket []
    
    putStrLn $ identity ++ ": " ++ reply


-- Eventually we can Put all of this in a Single Data Type
backendFunc :: PollEvent -> Int -> [String] -> Int -> Socket a -> Socket b -> IO (Int, [String], Int)
backendFunc None a b c _ _ = return (a, b, c)
backendFunc In avail_workers workers_list client_nbr backend frontend = do 
    worker_addr <- receive backend []
    when (avail_workers >= nbrWorkers) $ error ""

    empty <- fmap unpack $ receive backend []
    when (empty /= "") $ error ""
    
    let avail' = avail_workers + 1
    let work_list = workers_list ++ [show avail']
    
    client_addr <- fmap unpack $ receive backend []
    if client_addr == "READY"
        then return (avail', work_list, client_nbr)
        else do
            empty' <- fmap unpack $ receive backend []
            when (empty' /= "") $ error ""
        
            reply <- receive backend []
        
            send frontend (pack client_addr) [SndMore]
            send frontend (pack "") [SndMore]
            send frontend reply []
            return (avail', work_list, client_nbr - 1)


frontendFunc :: PollEvent -> Int -> [String] -> Int -> Socket a -> Socket b -> IO (Int, [String])
frontendFunc None a b _ _ _ = return (a, b)
frontendFunc In 0 b _ _ _ = return (0, b)
frontendFunc In avail_workers workers_list client_nbr frontend backend = do
    client_addr <- receive frontend []
    empty <- fmap unpack $ receive frontend []
    when (empty /= "") $ error ""
    request <- receive frontend []
    
    let worker_id = head workers_list 
    
    send backend (pack $ show worker_id) [SndMore]
    send backend (pack "") [SndMore]
    send backend client_addr [SndMore]
    send backend (pack "") [SndMore]
    send backend request []
    
    return (avail_workers - 1, tail workers_list)
    

lruQueueFunc :: Int -> [String] -> Int -> Socket a -> Socket a1 -> IO ()
lruQueueFunc avail_workers workers_list client_nbr backend frontend = do
    [S backend' res1, S frontend' res2] <- poll [S backend In, S frontend In] (-1)
    (avail_workers', workers_list', client_nbr') <- backendFunc res1 avail_workers workers_list client_nbr backend frontend
    
    when (client_nbr' > 0) $ do
        (avail_workers'', workers_list'') <- frontendFunc res2 avail_workers' workers_list' client_nbr' frontend backend
        lruQueueFunc avail_workers'' workers_list'' client_nbr' backend frontend
     

main :: IO ()
main = do
    let url_worker = "inproc://workers"
        url_client = "inproc://clients"
        client_nbr = nbrClients
        
    withContext 1 $ \context -> do
        withSocket context XRep $ \frontend -> do
            bind frontend url_client
            withSocket context XRep $ \backend -> do
                bind backend url_worker
                forM_ [1..nbrWorkers] $ \i -> forkIO (workerThread url_worker context i)
                forM_ [1..nbrClients] $ \i -> forkIO (clientThread url_client context i)
                lruQueueFunc 0 [] client_nbr backend frontend
                threadDelay $ 1 * 1000 * 1000
                