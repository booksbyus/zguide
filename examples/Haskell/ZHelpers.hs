module ZHelpers where

import System.ZMQ4.Monadic

import Numeric (showHex)

import Control.Applicative ((<$>))
import System.Random
import System.Locale hiding (defaultTimeLocale)
import Data.Time
import Data.ByteString.Char8 (pack, unpack)
import Data.Char (ord)
import Text.Printf (printf)
import qualified Data.ByteString as B


type Frame = B.ByteString
type Message = [Frame]


dumpMsg :: [B.ByteString] -> IO ()
dumpMsg msg_parts = do
    putStrLn "----------------------------------------"
    mapM_ (\i -> putStr (printf "[%03d] " (B.length i)) >> func (unpack i)) msg_parts where
        func :: String -> IO ()
        func item | all (\c -> (ord c >= 32) && (ord c <= 128)) item = putStrLn item 
                  | otherwise   = putStrLn $ prettyPrint $ pack item

dumpSock :: Receiver t => Socket z t  -> ZMQ z ()
dumpSock sock = fmap reverse (receiveMulti sock) >>= liftIO . dumpMsg

-- In General Since We use Randomness, You should Pass in
-- an StdGen, but for simplicity we just use newStdGen
setRandomIdentity :: Socket z t -> ZMQ z ()
setRandomIdentity sock = liftIO genUniqueId >>= (\ident -> setIdentity (restrict $ pack ident) sock)


-- You probably want to use a ext lib to generate random unique id in production code
genUniqueId :: IO String
genUniqueId = do
    gen <- liftIO newStdGen
    let (val1, gen') = randomR (0 :: Int, 65536) gen
    let (val2, _) = randomR (0 :: Int, 65536) gen'
    return $ show val1 ++ show val2

-- In General Since We use Randomness, You should Pass in
-- an StdGen, but for simplicity we just use newStdGen
--genKBytes :: Int -> IO B.ByteString
--genKBytes k = do
--    gen <- newStdGen
--    bs <- foldM (\(g, s) _i -> let (val, g') = random g in return (g', cons val s)) (gen, empty) [1..k]
--    return $ snd bs

prettyPrint :: B.ByteString -> String
prettyPrint = concatMap (`showHex` "") . B.unpack

-- Returns the current UNIX time in milliseconds
currentTime_ms :: IO Integer
currentTime_ms = do
    time_secs <- (read <$> formatTime defaultTimeLocale "%s" <$> getCurrentTime) :: IO Integer
    return $ time_secs * 1000

nextHeartbeatTime_ms :: Integer -> IO Integer
nextHeartbeatTime_ms heartbeatInterval_ms = do
    currTime <- currentTime_ms
    return $ currTime + heartbeatInterval_ms


-- Message frames util functions
--

-- Push frame plus empty frame before first frame.
z_wrap :: Message -> Frame -> Message
z_wrap msg frame = frame : B.empty : msg

-- Pop frame and empty frame if follows
z_unwrap :: Message -> (Frame, Message)
z_unwrap msg = 
    let (frame, msg') = z_pop msg
        (_, msg'') = z_pop msg'
    in  (frame, msg'')

-- Push frame before all frames
z_push :: Message -> Frame -> Message
z_push msg frame = frame : msg

-- Pop first frame from message.
-- Returns an empty frame/message pair if there's nothing in the message in order to
-- make more convenient the construction of new messages on the fly.
-- TODO: Think how to put a Maybe in here.
z_pop :: Message -> (Frame, Message)
z_pop [] = (B.empty, [B.empty])
z_pop [frame] = (frame, [B.empty])
z_pop (frame:rest) = (frame, rest)
