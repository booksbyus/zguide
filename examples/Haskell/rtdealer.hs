module Main where
    
import System.ZMQ
import Control.Concurrent (forkIO, threadDelay)
import Data.ByteString.Char8 (pack, unpack)
import System.Random (newStdGen, randomR, StdGen)
import Control.Monad (foldM_)

workerTaskA :: Context -> IO ()
workerTaskA ctx = withSocket ctx XReq $ \worker -> do
    setOption worker (Identity "A")
    connect worker "ipc://routing.ipc"
    recv_func 0 worker where
        recv_func val sock = do
            res <- receive sock []
            if unpack res == "END" then putStrLn ("A received: " ++ show val)
                else recv_func (val+1) sock
        
workerTaskB :: Context -> IO ()
workerTaskB ctx = withSocket ctx XReq $ \worker -> do
    setOption worker (Identity "B")
    connect worker "ipc://routing.ipc"
    recv_func 0 worker where
        recv_func val sock = do
            res <- receive sock []
            if unpack res == "END" then putStrLn ("B received: " ++ show val)
                else recv_func (val+1) sock

main :: IO ()
main = withContext 1 $ \context -> do
    withSocket context XRep $ \client -> do
        bind client "ipc://routing.ipc"
        forkIO (workerTaskA context)
        forkIO (workerTaskB context)
        threadDelay $ 1 * 1000 * 1000

        gen <- newStdGen
        
        let func g _i = do
            let (val, g') = randomR (0, 2) g :: (Int, StdGen)
            if val > 0
                then send client (pack "A") [SndMore]
                else send client (pack "B") [SndMore]
            send client (pack "This is the workload") []
            return g'
            
        foldM_ func gen [1..10]
                
        send client (pack "A") [SndMore]
        send client (pack "END") []
        
        send client (pack "B") [SndMore]
        send client (pack "END") []
        
        -- Give 0MQ/2.0.x time to flush output
        threadDelay $ 1 * 1000 * 1000