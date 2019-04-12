{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server.Handlers(heroeHandlers) where

import Data.Time (UTCTime)
import Data.Text
import Data.Maybe
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Vector as V

import Servant.Server
import Servant.API
import Heroe.Types
import Server.Types
import CSVHeroe

heroesScore :: String ->  Handler [Heroe]
heroesScore heroesFilePath = do
  heroes <- liftIO $ getHeroesFromFile heroesFilePath
  pure $ V.toList heroes

heroeHandlers :: String -> Server HeroeAPI
heroeHandlers = heroesScore
