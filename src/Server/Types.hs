{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server.Types where

import Data.Text
import Data.Aeson
import Servant.API
import Heroe.Types

instance FromJSON Heroe
instance ToJSON Heroe

type HeroeAPI = "score" :> Get '[JSON] [Heroe]
