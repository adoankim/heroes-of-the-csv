{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module CSVHeroe where

import Data.Csv
import Control.Exception
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL

import CSVHeroe.Types
import Heroe.Types

byteStringToHeroesVector :: BL.ByteString -> IO (Maybe HeroesVector)
byteStringToHeroesVector csvData = case decodeByName csvData of
  Left err -> do
    putStrLn $ "Couldn't decode csv file " ++ err
    pure Nothing
  Right (h, v) -> pure $ Just (h, v)

vectorHeroeOrEmpty :: Maybe HeroesVector -> V.Vector Heroe
vectorHeroeOrEmpty maybeHeroes = fromMaybe V.empty (snd <$> maybeHeroes)

getHeroesFromFile :: String -> IO(V.Vector Heroe)
getHeroesFromFile fileName = do
  dataOrErr <- (try $ BL.readFile fileName) :: IO(Either IOError BL.ByteString)
  case dataOrErr of
    Left _ -> do
      putStrLn ("Cannot read from " ++ fileName ++ " file.")
      pure V.empty
    Right csvData -> do
      maybeHeroes <- byteStringToHeroesVector csvData
      pure $ vectorHeroeOrEmpty maybeHeroes
