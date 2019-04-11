{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server(app) where

import qualified Data.Vector as V
import Network.Wai
import Servant
import Heroe.Types
import Server.Types
import Server.Handlers

app :: V.Vector Heroe -> Application
app heroes = serve (Proxy :: Proxy HeroeAPI) (heroeHandlers heroes)
