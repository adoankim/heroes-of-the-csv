{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server(app) where

import Network.Wai
import Servant
import Server.Types
import Server.Handlers

app :: Application
app = serve (Proxy :: Proxy HeroeAPI) heroeHandlers
