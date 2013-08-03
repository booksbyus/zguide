{-# LANGUAGE OverloadedStrings #-}
-- |
-- Weather broadcast server in Haskell
-- Binds SUB socket to tcp://localhost:5556
-- Collects weather updates and finds avg temp in zipcode
-- 

module Main where

import System.ZMQ4.Monadic
import Control.Monad (replicateM)
import Control.Applicative((<$>), (<*>))
import Data.ByteString.Char8 (unpack, pack)
import Text.Printf
import qualified Control.Foldl as L

-- | Average uses applicative with `Control.Foldl` in order to traverse the list only once.
average :: L.Fold Double Double
average = (/) <$> L.sum <*> L.genericLength

mapped :: (a -> b) -> L.Fold b r -> L.Fold a r
mapped f (L.Fold step begin done) = L.Fold step' begin done
  where
    step' x = step x . f

main :: IO ()
main =
  runZMQ $ do
    subscriber <- socket Sub
    connect subscriber "tcp://localhost:5556"
    -- Subscribe for NY City zipcode
    subscribe subscriber (pack "10001")
        
    records <- replicateM 5 $ do
      update <- receive subscriber
      let [_, temp, hum] = map read $ words $ unpack update
      return (temp, hum)

    liftIO $ printf "NY City: avg temperature of %.1f°C and avg humidity of %.1f%%\n" (avgTemp records) (avgHum records) 

    where
       avgTemp = L.fold (mapped fst average) 
       avgHum  = L.fold (mapped snd average) 
