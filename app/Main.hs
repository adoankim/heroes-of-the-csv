module Main where

import Lib
import Server
import Data.Maybe
import qualified Data.Vector as V
import Network.Wai.Handler.Warp

main :: IO ()
main = do
  heroes <- readCSVFile "heroes.csv" False
  putStrLn "Serving scores on port 3001"

  run 3001 (app $ fromMaybe V.empty heroes)
