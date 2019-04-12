module Main where

import Lib
import Server
import Network.Wai.Handler.Warp

main :: IO ()
main = do
  readCSVFile "heroes.csv" False
  putStrLn "Serving scores on port 3001"
  run 3001 $ app "heroes.csv"
