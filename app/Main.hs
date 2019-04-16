module Main where

import Lib
import Server
import Network.Wai.Handler.Warp
import Control.Monad.Trans.Reader

main :: IO ()
main = do
  readCSVFile "heroes.csv" False
  putStrLn "Serving scores on port 3001"
  app' <- runReaderT app "heroes.csv"
  run 3001 app'
