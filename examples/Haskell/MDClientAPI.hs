{-
    Majordomo Client API
-}
module MDClientAPI
    (   withMDCli
    ,   mdSetTimeout
    ,   mdSetRetries
    ,   mdSend
    ) where


import ZHelpers
import MDPDef
import System.ZMQ4

import Control.Monad (when, liftM)
import Control.Applicative ((<$>))
import Control.Exception (bracket)
import Data.ByteString.Char8 (pack, unpack, empty, ByteString(..))
import qualified Data.List.NonEmpty as N


data ClientAPI = ClientAPI {
      ctx :: Context
    , broker :: String
    , client :: Socket Req
    , verbose :: Bool
    , timeout :: Integer
    , retries :: Int
    }


withMDCli :: String -> Bool -> (ClientAPI -> IO a) -> IO a
withMDCli broker verbose act =
    bracket (mdInit broker verbose)
            (mdDestroy)
            act

{-
    Public API
-}
mdSetTimeout :: ClientAPI -> Integer -> IO ClientAPI
mdSetTimeout api newTimeout = return api { timeout = newTimeout }

mdSetRetries :: ClientAPI -> Int -> IO ClientAPI
mdSetRetries api newRetries = return api { retries = newRetries }

-- Sends a request to the broker and retries for a reply until it can
mdSend :: ClientAPI -> String -> Message -> IO Message
mdSend api service request = do
    -- Protocol frames
    -- Frame 1: "MDPCxy" (six bytes, MDP/Client x.y)
    -- Frame 2: Service name (printable string)
    let wrappedRequest = [mdpcClient, pack service] ++ request
    when (verbose api) $ do
        putStrLn $ "I: Send request to " ++ service ++ " service:"
        dumpMsg wrappedRequest 
   
    trySend (client api) wrappedRequest
  where trySend :: Socket Req -> Message -> IO Message
        trySend clientSock wrappedRequest = do
            sendMulti clientSock (N.fromList wrappedRequest)

            [evts] <- poll (fromInteger $ timeout api) [Sock clientSock [In] Nothing] 
            if In `elem` evts
            then do
                msg <- receiveMulti clientSock
                when (verbose api) $ do
                    putStrLn "I: received reply"
                    dumpMsg msg

                when (length msg < 3) $ do
                    putStrLn "E: Malformed message"
                    dumpMsg msg
                    error ""

                let (header, msg') = z_pop msg
                    (reply_service, msg'') = z_pop msg'
                when (header /= mdpcClient) $
                    error $ "E: Malformed header '" ++ (unpack header) ++ "'"
                when ((unpack reply_service) /= service) $
                    error $ "E: Malformed service '" ++ (unpack reply_service) ++ "'"

                return msg''
            else
                if retries api > 0
                then do
                    when (verbose api) $ putStrLn "W: No reply, reconnecting..."
                    newAPI <- mdConnectToBroker api
                    mdSend (newAPI { retries = ((retries api)-1) }) service (drop 2 wrappedRequest)
                else do 
                    when (verbose api) $ putStrLn "W: Permanent error, abandoning"
                    error ""


{-
    Private API
-}
mdConnectToBroker :: ClientAPI -> IO ClientAPI
mdConnectToBroker api = do
    close $ client api
    reconnectedClient <- socket (ctx api) Req
    connect reconnectedClient (broker api)
    when (verbose api) $ do
        putStrLn $ "I: connecting to broker at " ++ (broker api)
    return api { client = reconnectedClient }

mdInit :: String -> Bool -> IO ClientAPI
mdInit broker verbose = do
    ctx <- context
    client <- socket ctx Req
    let newAPI = ClientAPI { ctx = ctx
                           , client = client
                           , broker = broker
                           , verbose = verbose
                           , timeout = 2500
                           , retries = 3
                           }
    mdConnectToBroker newAPI

mdDestroy :: ClientAPI -> IO ()
mdDestroy api = do
    close (client api)
    shutdown (ctx api)
