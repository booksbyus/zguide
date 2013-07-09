module Main where

import System.ZMQ3.Monadic (runZMQ, socket, bind, receive, moreToReceive, proxy, send, poll, Poll(..), Event(..), Router(..), Dealer(..), Socket, Timeout, Flag (SendMore), liftIO)
import Control.Monad (forever)    

main :: IO ()
main = 
    runZMQ $ do 
        frontend <- socket Router
        bind frontend "tcp://*:5559"
        backend <- socket Dealer
        bind backend "tcp://*:5560"
        proxy frontend backend Nothing
        --TODO I still need to find out how to use the new version of poll
        --forever $ liftIO $ loopFunction frontend backend (-1)

--loopFunction :: Socket a -> Socket b -> Timeout -> IO ()
--loopFunction front back timeout = do
--    [[]] <- poll timeout [Sock front [In] Nothing, Sock back [In] Nothing]
--    return()
    --case evts of
    --    Nothing -> return()
    --    evt : [next] ->  process front back evt

--process :: Socket a -> Socket b -> Event -> IO ()
--process sock_recv sock_to_send Err = return ()
--process sock_recv sock_to_send In = do
--    msg <- receive sock_recv
--    more <- moreToReceive sock_recv
--    if more 
--        then do
--            send sock_to_send [SendMore] msg
--            process sock_recv sock_to_send In
--        else send sock_to_send [] msg
