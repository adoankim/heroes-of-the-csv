{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server.Handlers(heroeHandlers) where

import Data.Time (UTCTime)
import Data.Text
import Servant.Server
import Servant.API
import Heroe.Types
import Server.Types

heroesScore :: Handler [Heroe]
heroesScore = pure []

heroeHandlers :: Server HeroeAPI
heroeHandlers = heroesScore
