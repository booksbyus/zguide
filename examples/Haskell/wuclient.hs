{-# LANGUAGE OverloadedStrings #-}
-- |
-- Weather broadcast server in Haskell
-- Binds SUB socket to tcp://localhost:5556
-- Collects weather updates and finds avg temp in zipcode
-- 
-- Translated to Haskell by ERDI Gergo http://gergo.erdi.hu/

module Main where

import System.ZMQ3.Monadic
import Control.Monad (replicateM)
import Data.ByteString.Char8 (unpack, pack)
import System.Environment (getArgs)

main :: IO ()
main = do    
  args <- getArgs
  let zipcode = case args of
        [zipcode] -> zipcode
        _ -> "10001"
      
  runZMQ $ do
      subscriber <- socket Sub
      connect subscriber "tcp://localhost:5556"
      subscribe subscriber (pack zipcode)
            
      temperatures <- replicateM 5 $ do
          update <- receive subscriber
          let [zipcode, temperature, humidity] = map read $ words $ unpack update
          return temperature
      
      let avgTemp = fromIntegral (sum temperatures) / fromIntegral (length temperatures)
      liftIO $ putStrLn $ unwords ["Average temperature for zipcode", zipcode, "was", show avgTemp]
