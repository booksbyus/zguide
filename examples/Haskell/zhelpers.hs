module ZHelpers where

import System.ZMQ
import System.Random

import Numeric (showHex)
import Control.Monad (foldM)
import Control.Applicative ((<*>))
import Data.ByteString.Char8 (pack, unpack, ByteString, cons, empty)
import Data.Char (ord)
import Text.Printf (printf)
import qualified Data.ByteString as B

-- Not Tail Recursive
recvMultipart :: Socket a -> IO [String]
recvMultipart sock = do
    recv <- fmap unpack $ receive sock []
    mre <- moreToReceive sock
    if mre
        then do
            remainder <- recvMultipart sock
            return $ recv : remainder
        else return [recv] 

sendMultipart :: [String] -> Socket a -> IO ()
sendMultipart [] sock = return ()
sendMultipart [x] sock = send sock (pack x) []
sendMultipart (x:xs) sock = send sock (pack x) [SndMore] >> sendMultipart xs sock


dumpMsg :: [B.ByteString] -> IO ()
dumpMsg msg_parts = do
    putStrLn "----------------------------------------"
    mapM_ (\i -> putStr (printf "[%03d] " (B.length i)) >> func (unpack i)) msg_parts where
        func :: String -> IO ()
        func item | all (\c -> (ord c >= 32) && (ord c <= 128)) item = putStrLn item 
                  | otherwise   = putStrLn $ prettyPrint $ pack item

dumpSock :: Socket a -> IO ()
dumpSock sock = fmap reverse (recvMultipart sock) >>= dumpMsg . map pack


-- In General Since We use Randomness, You should Pass in
-- an StdGen, but for simplicity we just use newStdGen
setId :: Socket a -> IO ()
setId sock = do
    gen <- newStdGen
    let (val1, gen') = randomR (0 :: Int, 65536) gen
    let (val2, gen'') = randomR (0 :: Int, 65536) gen'
    let string_id = show val1 ++ show val2
    setOption sock (Identity string_id)

zpipe :: Context -> (Socket Pair -> Socket Pair -> IO a) -> IO a
zpipe ctx func = withSocket ctx Pair $ \sock_a -> do
    setOption sock_a (Linger 0)
    setOption sock_a (HighWM 1)
    withSocket ctx Pair $ \sock_b -> do
        setOption sock_b (Linger 0)
        setOption sock_b (HighWM 1)
        bytes <- genKBytes 8
        let iface = "inproc://" ++ prettyPrint bytes
        bind sock_a iface
        connect sock_b iface
        func sock_a sock_b

-- In General Since We use Randomness, You should Pass in
-- an StdGen, but for simplicity we just use newStdGen
genKBytes :: Int -> IO B.ByteString
genKBytes k = do
    gen <- newStdGen
    bs <- foldM (\(g, s) _i -> let (val, g') = random g in return (g', cons val s)) (gen, empty) [1..k]
    return $ snd bs

prettyPrint :: B.ByteString -> String
prettyPrint = concatMap (`showHex` "") . B.unpack