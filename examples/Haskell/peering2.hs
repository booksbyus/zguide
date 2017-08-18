{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent     (threadDelay)
import           Control.Monad          (forM_, forever, void, when)
import           Control.Monad.IO.Class

import qualified Data.ByteString.Char8  as C
import           Data.List              (find)
import           Data.List.NonEmpty     (NonEmpty (..), (<|))
import qualified Data.List.NonEmpty     as N
import           Data.Semigroup         ((<>))
import           Data.Sequence          (Seq, ViewL (..), viewl, (|>))
import qualified Data.Sequence          as S

import           System.Environment
import           System.Exit
import           System.Random

import           System.ZMQ4.Monadic

workerNum :: Int
workerNum = 3

clientNum :: Int
clientNum = 10

-- | The client task does a request-reply dialog using a standard
-- synchronous REQ socket.
clientTask :: Show a => String -> a -> ZMQ z ()
clientTask self i = do
    client <- socket Req
    connect client (connectString self "localfe")
    let ident = "Client-" <> C.pack self <> C.pack (show i)
    setIdentity (restrict ident) client
    forever $ do
        send client [] "HELLO"
        reply <- receiveMulti client
        liftIO $ do
            C.putStrLn $ "Client: " <> C.pack (show reply)
            threadDelay 10000

-- | The worker task plugs into the load-balancer using a REQ socket
workerTask :: Show a => String -> a -> ZMQ z ()
workerTask self i = do
    worker <- socket Req
    connect worker (connectString self "localbe")
    let ident = "Worker-" <> C.pack self <> C.pack (show i)
    setIdentity (restrict ident) worker
    send worker [] "READY"
    forever $ do
        msg <- receiveMulti worker
        liftIO $ print (ident, "Sending"::String, msg)
        sendMulti worker (replaceLast "OK" msg)

-- | This is similar to zframe_reset(zmsg_last (msg), ..) in czmq.
replaceLast :: a -> [a] -> NonEmpty a
replaceLast y (_:[]) = y :| []
replaceLast y (x:xs) = x <| replaceLast y xs
replaceLast y [] = y :| []

-- | Connect a peer using the connectString function
connectPeer :: Socket z t -> String -> String -> ZMQ z ()
connectPeer sock name p = connect sock (connectString p name)

-- | An ipc connection string
connectString :: String -> String -> String
connectString peer name = "ipc://" ++ peer ++ "-" ++ name ++ ".ipc"

type Workers = Seq C.ByteString

-- | Interesting part
-- Here, we handle the request-reply flow. We're using load-balancing
-- to poll workers at all times, and clients only when there are one
-- or more workers available.
clientWorkerPoll
    :: (Receiver t1, Receiver t2, Receiver t3, Receiver t4, Sender t1, Sender t2, Sender t3, Sender t4)
    => Socket z t1
    -> Socket z t2
    -> Socket z t3
    -> Socket z t4
    -> [String]
    -> ZMQ z ()
clientWorkerPoll
    localBack
    cloudBack
    localFront
    cloudFront
    peers = loop S.empty -- Queue of workers starts empty
  where
    loop workers = do
        -- Poll backends, if we have no workers, wait indefinitely
        [localEvents, cloudEvents] <- poll (if S.length workers > 0 then oneSec else -1) backends

        availableWorkers <- reqRep workers localEvents cloudEvents
        availableWorkers' <- workerLoop availableWorkers

        loop availableWorkers'

    reqRep workers local cloud
        -- Handle reply from local worker
        | In `elem` local = do
            msg <- receiveMulti localBack
            case msg of
                -- Worker is READY, don't route the message further
                ident:_:"READY":_ -> return (workers |> ident)
                -- Worker replied
                ident:_:restOfMsg -> do
                    route restOfMsg
                    return (workers |> ident)
                -- Something strange happened
                m -> do
                    liftIO $ print m
                    return workers
        -- Handle reply from peer broker
        | In `elem` cloud = do
            msg <- receiveMulti cloudBack
            case msg of
                -- We don't use the peer broker identity for anything
                _:restOfMsg -> route restOfMsg
                -- Something strange happened
                m -> liftIO $ print m
            return workers
        | otherwise = return workers

    route msg@(ident:_) = do
        let msg' = N.fromList msg
            peer = find (== ident) bPeers
        case peer of
            -- Route reply to cloud if it's addressed to a broker
            Just _ -> sendMulti cloudFront msg'
            -- Route reply to local client
            Nothing -> sendMulti localFront msg'
    route m = liftIO $ print m -- Something strange happened

    -- Now, we route as many client requests as we have worker capacity
    -- for. We may reroute requests from our local frontend, but not from
    -- the cloud frontend. We reroute randomly now, just to test things 
    -- out. In the next version, we'll do this properly by calculating
    -- cloud capacity.
    workerLoop workers = if S.null workers
        then return workers
        else do
            [localEvents, cloudEvents] <- poll 0 frontends
            routeRequests workers localEvents cloudEvents

    routeRequests workers local cloud
        -- We'll do peer brokers first, to prevent starvation
        | In `elem` cloud = do
            msg <- receiveMulti cloudFront
            rerouteReqs workers (Left msg)
        | In `elem` local = do
            msg <- receiveMulti localFront
            rerouteReqs workers (Right msg)
        -- No work, go back to backends
        | otherwise = return workers

    -- If rerouteable, send to cloud 20% of the time
    -- Here we'd normally use cloud status information
    --
    -- Right denotes rerouteable. Left denotes not-rerouteable.
    rerouteReqs workers (Right msg) = do
        cont <- liftIO $ randomRIO (0::Int,4)
        if cont == 0
            then do
                -- Route to random broker peer
                p <- liftIO $ randomRIO (0, length peers - 1)
                let randomPeer = bPeers !! p
                liftIO $ print ("Sending to random peer"::String, randomPeer)
                sendMulti cloudBack (randomPeer :| msg)
                return workers
            else rerouteReqs workers (Left msg)
    rerouteReqs workers (Left msg) = do
        let (worker, newWorkers) = popWorker (viewl workers)
        case worker of
            Nothing -> workerLoop newWorkers
            Just w -> do
                sendMulti localBack $ w :| [""] ++ msg
                return newWorkers

    oneSec = 1000
    bPeers = map C.pack peers

    backends =
        [ Sock localBack [In] Nothing
        , Sock cloudBack [In] Nothing ]
    frontends =
        [ Sock localFront [In] Nothing
        , Sock cloudFront [In] Nothing ]

    popWorker EmptyL = (Nothing, S.empty)
    popWorker (l :< s) = (Just l, s)

main :: IO ()
main = do
    args <- getArgs
    when (length args < 2) $ do
        putStrLn "Usage: broker <me> <you> [<you> ...]"
        exitFailure

    -- First argument is this broker's name
    -- Other arguments are our peers' names
    let self:peers = args
    putStrLn $ "Preparing broker at " ++ self

    runZMQ $ do
        -- Bind cloud frontend to endpoint
        cloudFront <- socket Router
        setIdentity (restrict (C.pack self)) cloudFront
        bind cloudFront (connectString self "cloud")

        -- Connect cloud backend to all peers
        cloudBack <- socket Router
        setIdentity (restrict (C.pack self)) cloudBack
        mapM_ (connectPeer cloudBack "cloud") peers

        -- Prepare local frontend and backend
        localFront <- socket Router
        bind localFront (connectString self "localfe")
        localBack <- socket Router
        bind localBack (connectString self "localbe")

        -- Get user to tell us when we can start...
        liftIO $ do
            putStrLn "Press Enter when all brokers are started."
            void getLine

        -- Start workers and clients
        forM_ [1..workerNum] $ async . workerTask self
        forM_ [1..clientNum] $ async . clientTask self

        -- Request reply flow
        clientWorkerPoll
            localBack
            cloudBack
            localFront
            cloudFront
            peers
