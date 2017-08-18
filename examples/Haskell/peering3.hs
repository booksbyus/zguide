{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent               (threadDelay)
import           Control.Monad                    (forM_, forever, when)
import           Control.Monad.IO.Class

import           Data.Attoparsec.ByteString.Char8 hiding (take)
import qualified Data.ByteString.Char8            as C
import           Data.List                        (find, unfoldr)
import           Data.List.NonEmpty               (NonEmpty (..))
import qualified Data.List.NonEmpty               as N
import           Data.Semigroup                   ((<>))
import           Data.Sequence                    (Seq, ViewL (..), viewl, (|>))
import qualified Data.Sequence                    as S

import           System.Environment
import           System.Exit
import           System.Random

import           System.ZMQ4.Monadic

workerNum :: Int
workerNum = 5

clientNum :: Int
clientNum = 10

-- | This is the client task. It issues a burst of requests and then
-- sleeps for a few seconds. This simulates sporadic activity; when
-- a number of clients are active at once, the local workers should
-- be overloaded. The client uses a REQ socket for requests and also
-- pushes statistics over the monitor socket.
clientTask :: Show a => String -> a -> ZMQ z ()
clientTask self i = do
    client <- socket Req
    connect client (connectString self "localfe")
    mon <- socket Push
    connect mon (connectString self "monitor")
    let ident = "Client-" <> C.pack self <> C.pack (show i)
    setIdentity (restrict ident) client

    forever $ do
        -- Sleep random amount. 0 to 4 seconds.
        liftIO $ randomRIO (0,4000000) >>= threadDelay
        numTasks <- liftIO $ randomRIO (0,14)
        g <- liftIO newStdGen
        let taskIds :: [Int]
            taskIds = take numTasks $ unfoldr (Just . randomR (0,0x10000)) g
            pollset taskId = [ Sock client [In] (Just $ const $ receivedReply taskId) ]
            receivedReply taskId = do
                reply <- receive client
                -- Worker is supposed to answer us with our task ID
                when (taskId /= reply) $ 
                    liftIO $ print (reply, taskId)
                send mon [] reply

        forM_ taskIds $ \taskId -> do
            -- Send request with random ID
            let bTaskId = C.pack (show taskId)
            send client [] bTaskId

            -- Wait max ten seconds for a reply, then complain
            [pollEvt] <- poll 10000 (pollset bTaskId)
            when (null pollEvt) $
                send mon [] $ "Client exit - lost task " <> bTaskId

-- | This is the worker task, which uses a REQ socket to plug into the
-- load-balancer. It's the same stub worker task that you've seen in
-- other examples.
workerTask :: Show a => String -> a -> ZMQ z ()
workerTask self i = do
    worker <- socket Req
    connect worker (connectString self "localbe")
    let ident = "Worker-" <> C.pack self <> C.pack (show i)
    setIdentity (restrict ident) worker

    -- Tell broker we're ready for work
    send worker [] "READY"

    -- Process messages as they arrive
    forever $ do
        msg <- receiveMulti worker
        -- Workers are busy for 0-1 seconds
        liftIO $ randomRIO (0,1000000) >>= threadDelay
        sendMulti worker (N.fromList msg)

-- | Connect a peer using the connectString function
connectPeer :: Socket z t -> String -> String -> ZMQ z ()
connectPeer sock name p = connect sock (connectString p name)

-- | An ipc connection string
connectString :: String -> String -> String
connectString peer name = "ipc://" ++ peer ++ "-" ++ name ++ ".ipc"

type Workers = Seq C.ByteString

-- | The main loop has two parts. First, we poll workers and our two service
-- sockets (stateFront and mon), in any case. If we have no ready workers,
-- then there's no point in looking at incoming requests. These can remain
-- on their internal 0MQ queues.
clientWorkerPoll
    ::  ( Receiver t1
        , Receiver t2
        , Receiver t4
        , Receiver t5
        , Receiver t6
        , Receiver t7
        , Sender t1
        , Sender t2
        , Sender t3
        , Sender t4
        , Sender t5 )
    => String
    -> Socket z t1
    -> Socket z t2
    -> Socket z t3
    -> Socket z t4
    -> Socket z t5
    -> Socket z t6
    -> Socket z t7
    -> [String]
    -> ZMQ z ()
clientWorkerPoll
    self
    localBack
    cloudBack
    stateBack
    localFront
    cloudFront
    stateFront
    mon
    peers = loop S.empty 0 -- Queue of workers starts empty
  where
    loop workers cloudCapacity = do
        -- Poll primary, if we have no workers, wait indefinitely
        [localEvents, cloudEvents, stateEvents, _] <- poll (if S.length workers > 0 then oneSec else -1) primary

        availableWorkers <- reqRep workers localEvents cloudEvents

        -- If we have input messages on the stateFront socket,
        -- process it immediately.
        cloudCapacity' <- if In `elem` stateEvents
            then stateChange cloudCapacity
            else return cloudCapacity

        availableWorkers' <- workerLoop workers availableWorkers cloudCapacity'

        loop availableWorkers' cloudCapacity'

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
                _ -> return workers
        -- Handle reply from peer broker
        | In `elem` cloud = do
            msg <- receiveMulti cloudBack
            case msg of
                -- We don't use the peer broker identity for anything
                _:restOfMsg -> route restOfMsg
                -- Something strange happened
                _ -> return ()
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
    route _ = return () -- Something strange happened

    -- Now, we route as many client requests as we can handle. If we have
    -- local capacity, we poll both localFront and cloudFront. If we have
    -- cloud capacity only, we poll just localFront. We route any request
    -- locally if we can, else we route to the cloud.
    workerLoop oldWorkers workers cloudCapacity = if areWorkers || areCloud
        then do
            evts <- poll 0 ((if areWorkers then id else take 1) secondary)
            case evts of
                [localEvents] ->
                    routeRequests oldWorkers workers cloudCapacity localEvents []
                [localEvents, cloudEvents] ->
                    routeRequests oldWorkers workers cloudCapacity localEvents cloudEvents
                _ -> return workers
        else return workers
      where
        areWorkers = not (S.null workers)
        areCloud = cloudCapacity > 0

    routeRequests oldWorkers workers cloudCapacity local cloud
        | In `elem` local =
            receiveMulti localFront >>= rerouteReqs oldWorkers workers cloudCapacity
        | In `elem` cloud =
            receiveMulti cloudFront >>= rerouteReqs oldWorkers workers cloudCapacity
        -- No work, go back to primary
        | otherwise = return workers

    rerouteReqs oldWorkers workers cloudCapacity msg = do
        newWorkers <- if S.null workers
            then do
                -- Route to random broker peer
                p <- liftIO $ randomRIO (0, length peers - 1)
                let randomPeer = bPeers !! p
                sendMulti cloudBack (randomPeer :| msg)
                return workers
            else do
                let (worker, newWorkers) = popWorker (viewl workers)
                case worker of
                    Nothing -> return ()
                    Just w -> sendMulti localBack $ w :| [""] <> msg
                return newWorkers

        -- We broadcast capacity messages to other peers; to reduce chatter,
        -- we do this only if our capacity changed.
        when (S.length oldWorkers /= S.length newWorkers) $
            sendMulti stateBack $ C.pack self :| [C.pack . show . S.length $ newWorkers]

        workerLoop oldWorkers newWorkers cloudCapacity

    oneSec = 1000
    bPeers = map C.pack peers

    -- If the state changed, update the cloud capacity.
    stateChange cloudCapacity = do
        msg <- receiveMulti stateFront
        case msg of
            _:status:_ -> do
                -- If we can't parse, assume 0...
                let statusNum = either (const 0) id (parseOnly decimal status)
                return (statusNum :: Int)
            _ -> return cloudCapacity -- Could not parse message

    primary =
        [ Sock localBack [In] Nothing
        , Sock cloudBack [In] Nothing
        , Sock stateFront [In] Nothing
        -- If we have messages on the monitor socket, process it immediately
        , Sock mon [In] (Just $ const $ receive mon >>= liftIO . C.putStrLn) ]
    secondary =
        [ Sock localFront [In] Nothing
        , Sock cloudFront [In] Nothing ]

    popWorker EmptyL = (Nothing, S.empty)
    popWorker (l :< s) = (Just l, s)

-- | The main task begins by setting up all its sockets. The local frontend
-- talks to clients, and our local backend talks to workers. The cloud
-- frontend talks to peer brokers as if they were clients, and the cloud
-- backend talks to peer brokers as if they were workers. The state
-- backend publishes regular state messages, and the state frontend
-- subscribes to all state backends to collect these messages. Finally,
-- we use a PULL monitor socket to sollect printable messages from tasks.
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
        -- Prepare local frontend and backend
        localFront <- socket Router
        bind localFront (connectString self "localfe")
        localBack <- socket Router
        bind localBack (connectString self "localbe")

        -- Bind cloud frontend to endpoint
        cloudFront <- socket Router
        setIdentity (restrict (C.pack self)) cloudFront
        bind cloudFront (connectString self "cloud")

        -- Connect cloud backend to all peers
        cloudBack <- socket Router
        setIdentity (restrict (C.pack self)) cloudBack
        mapM_ (connectPeer cloudBack "cloud") peers

        -- Bind state backend to endpoint
        stateBack <- socket Pub
        bind stateBack (connectString self "state")

        -- Connect state frontend to all peers
        stateFront <- socket Sub
        subscribe stateFront ""
        mapM_ (connectPeer stateFront "state") peers

        -- Prepare monitor socket
        mon <- socket Pull
        bind mon (connectString self "monitor")

        -- Start workers and clients
        forM_ [1..workerNum] $ async . workerTask self
        forM_ [1..clientNum] $ async . clientTask self

        -- Request reply flow
        clientWorkerPoll
            self
            localBack
            cloudBack
            stateBack
            localFront
            cloudFront
            stateFront
            mon
            peers
