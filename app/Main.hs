module Main where

import Lib

main :: IO ()
main = readCSVFile "heroes.csv" False
