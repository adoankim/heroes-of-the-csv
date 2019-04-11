{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server.Handlers(heroeHandlers) where

import Data.Time (UTCTime)
import Data.Text
import Servant.Server
import Servant.API
import qualified Data.Vector as V
import Heroe.Types
import Server.Types

heroesScore :: V.Vector Heroe -> Handler [Heroe]
heroesScore heroes = pure $ V.toList heroes

heroeHandlers :: V.Vector Heroe -> Server HeroeAPI
heroeHandlers heroes = heroesScore heroes
