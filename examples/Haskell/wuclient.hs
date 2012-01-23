-- |
-- Weather broadcast server in Haskell
-- Binds SUB socket to tcp://localhost:5556
-- Collects weather updates and finds avg temp in zipcode
-- 
-- Translated to Haskell by ERDI Gergo http://gergo.erdi.hu/

module Main where

import System.ZMQ
import Control.Monad (replicateM)
import Data.ByteString.Char8 (unpack)
import System.Environment (getArgs)
import Data.String.Utils (splitWs)

main :: IO ()
main = do    
  args <- getArgs
  let zipcode = case args of
        [zipcode] -> zipcode
        _ -> "10001"
      
  withContext 1 $ \context -> do    
    withSocket context Sub $ \subscriber -> do
      connect subscriber "tcp://localhost:5556"
      subscribe subscriber zipcode
            
      temperatures <- replicateM 5 $ do
        update <- receive subscriber []
        let [zipcode, temperature, humidity] = map read $ splitWs $ unpack update
        return temperature
      
      let avgTemp = fromIntegral (sum temperatures) / fromIntegral (length temperatures)
      putStrLn $ unwords ["Average temperature for zipcode", zipcode, "was", show avgTemp]
