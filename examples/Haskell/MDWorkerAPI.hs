{-
    Majordomo Worker API
-}
module MDWorkerAPI
    (   withMDWorker
    ,   mdwkrExchange
    ,   mdwkrSetReconnect
    ,   mdwkrSetHeartbeat
    ) where


import System.ZMQ4
import ZHelpers
import MDPDef

import Control.Exception (bracket)
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Data.ByteString.Char8 (pack, unpack, empty, ByteString(..))
import Data.Maybe
import qualified Data.List.NonEmpty as N


data WorkerAPI = WorkerAPI {
      ctx :: Context
    , broker :: String
    , service :: String
    , worker :: Socket Dealer
    , verbose :: Bool

    -- Heartbeats
    , heartbeat_at :: Integer
    , liveness :: Int
    , heartbeatDelay_ms :: Integer
    , reconnectDelay_ms :: Integer

    , expect_reply :: Bool
    , reply_to :: Frame
    }

heartbeatLiveness :: Int
heartbeatLiveness = 3


{-
    Public API
-}
withMDWorker :: String -> String -> Bool -> (WorkerAPI -> IO a) -> IO a
withMDWorker broker service verbose session = do
    bracket (mdwkrInit broker service verbose) 
            (mdwkrDestroy)
            (session)

mdwkrInit :: String -> String -> Bool -> IO WorkerAPI
mdwkrInit broker service verbose = do
    ctx <- context
    worker <- socket ctx Dealer
    let newAPI = WorkerAPI { ctx = ctx
                           , broker = broker
                           , service = service
                           , worker = worker
                           , verbose = verbose
                           , heartbeat_at = 0
                           , liveness = 0
                           , reconnectDelay_ms = 2500
                           , heartbeatDelay_ms = 2500
                           , expect_reply = False
                           , reply_to = empty
                           }
    s_mdwkrConnectToBroker newAPI

-- Send reply to broker and wait for request
mdwkrExchange :: WorkerAPI -> Message -> IO (WorkerAPI, Message)
mdwkrExchange api reply = do
    when (reply /= [empty]) $ 
        s_mdwkrSendToBroker api mdpwReply Nothing (Just $ z_wrap reply (reply_to api)) 

    tryExchange api { expect_reply = True }
  where tryExchange :: WorkerAPI -> IO (WorkerAPI, Message)
        tryExchange api = do
            [evts] <- poll (fromInteger $ heartbeatDelay_ms api)
                           [Sock (worker api) [In] Nothing]

            if In `elem` evts
            then do
                msg <- receiveMulti $ worker api
                when (verbose api) $ do
                    putStrLn $ "I: Received message from broker: "
                    dumpMsg msg
                
                when (length msg < 3) $
                    error "E: Invalid message format"

                let (empty_, msg') = z_pop msg
                    (header, msg'') = z_pop msg'
                    (command, msg''') = z_pop msg''

                when (empty_ /= empty) $
                    error "E: Not an empty first frame"
                when (header /= mdpwWorker) $
                    error "E: Not a valid MDP header"

                case command of
                    cmd | cmd == mdpwRequest -> return (api { reply_to = fst . z_unwrap $ msg'''
                                                            , liveness = heartbeatLiveness
                                                            , expect_reply = True
                                                            }, 
                                                        msg''')
                        | cmd == mdpwDisconnect -> do
                              newAPI <- s_mdwkrConnectToBroker $ api { liveness = heartbeatLiveness
                                                                     , expect_reply = True }
                              tryExchange newAPI
                        | cmd == mdpwHeartbeat -> do
                              tryExchange api { liveness = heartbeatLiveness
                                              , expect_reply = True }
                        | otherwise -> do
                              putStrLn "E: Invalid input message"
                              dumpMsg msg
                              tryExchange api { liveness = heartbeatLiveness
                                              , expect_reply = True }
            else do
                if (liveness api == 0)
                then do
                    when (verbose api) $ do
                        putStrLn "W: Disconnected from broker - retrying..."
                    threadDelay ((fromInteger $ reconnectDelay_ms api) * 1000)
                    newAPI <- s_mdwkrConnectToBroker api
                    tryExchange newAPI { expect_reply = True }
                else do
                    currTime <- currentTime_ms
                    if (currTime > heartbeat_at api) 
                    then do
                        s_mdwkrSendToBroker api mdpwHeartbeat Nothing Nothing
                        nextHeartbeat <- nextHeartbeatTime_ms $ heartbeatDelay_ms api
                        tryExchange api { heartbeat_at = nextHeartbeat
                                        , expect_reply = True 
                                        , liveness = liveness api - 1 }
                    else tryExchange api { expect_reply = True 
                                         , liveness = liveness api - 1 }

mdwkrSetReconnect :: WorkerAPI -> Integer -> IO WorkerAPI
mdwkrSetReconnect api newReconnectDelay_ms = 
    return api { reconnectDelay_ms = newReconnectDelay_ms }

mdwkrSetHeartbeat :: WorkerAPI -> Integer -> IO WorkerAPI
mdwkrSetHeartbeat api newHeartbeatDelay_ms = 
    return api { heartbeatDelay_ms = newHeartbeatDelay_ms }

{-
    Private API
-}
mdwkrDestroy :: WorkerAPI -> IO ()
mdwkrDestroy api = do
    close (worker api)
    shutdown (ctx api)

{-
    Helper functions
-}
-- Wraps a message with the appropriate MDP data and sends it to the broker
s_mdwkrSendToBroker :: WorkerAPI -> Frame -> Maybe Frame -> Maybe Message -> IO ()
s_mdwkrSendToBroker api command option msg = do
    let args = [Just empty, Just mdpwWorker, Just command, option]
        msg' = fromMaybe [] msg
        wrappedMessage = (catMaybes args) ++ msg'
    when (verbose api) $ do
        let strCmd = mdpsCommands !! (mdpGetIdx . unpack $ command)
        putStrLn $ "I: Sending " ++ unpack strCmd ++ " to broker"
        dumpMsg wrappedMessage

    sendMulti (worker api) (N.fromList wrappedMessage)

-- Reconnects the worker to the broker and sends a READY message.
s_mdwkrConnectToBroker :: WorkerAPI -> IO WorkerAPI
s_mdwkrConnectToBroker api = do
    close $ worker api
    reconnectedWorker <- socket (ctx api) Dealer 
    connect reconnectedWorker (broker api)
    when (verbose api) $ do
        putStrLn $ "I: connecting to broker at " ++ (broker api)
    s_mdwkrSendToBroker api { worker = reconnectedWorker } 
                        mdpwReady (Just . pack $ service api) Nothing

    nextHeartbeat <- nextHeartbeatTime_ms $ heartbeatDelay_ms api
    return api { worker = reconnectedWorker
               , liveness = heartbeatLiveness 
               , heartbeat_at = nextHeartbeat
               }
