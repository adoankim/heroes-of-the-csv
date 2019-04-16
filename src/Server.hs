{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server(app) where

import Network.Wai
import Servant
import Server.Types
import Server.Handlers
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader

app :: MonadIO m => ReaderT String m Application
app = do
  heroeHandlers' <- heroeHandlers
  pure $ serve (Proxy :: Proxy HeroeAPI) heroeHandlers'
