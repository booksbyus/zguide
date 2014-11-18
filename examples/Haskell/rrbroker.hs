-- |
-- Simple message queuing broker
--
-- Use it with `rrclient.hs` and `rrworker.hs`//
module Main where

import System.ZMQ4.Monadic
import Control.Monad (forever)
import qualified Data.List.NonEmpty as NE -- from semigroups

main :: IO ()
main = runZMQ $ do
  frontend <- socket Router
  bind frontend "tcp://*:5559"
  backend <- socket Dealer
  bind backend "tcp://*:5560"
  forever $ poll (-1) [ Sock frontend [In] (Just $ frontend >|> backend)
                      , Sock backend [In] (Just $ backend >|> frontend)
                      ]

(>|>) :: (Receiver r, Sender s) => Socket z r -> Socket z s -> [Event] -> ZMQ z ()
(>|>) rcv snd _ = receiveMulti rcv >>= sendMulti snd . NE.fromList
