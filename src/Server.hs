{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server(app) where

import Network.Wai
import Servant
import Server.Types
import Server.Handlers

app :: String -> Application
app heroesFilePath = serve (Proxy :: Proxy HeroeAPI) (heroeHandlers heroesFilePath)
