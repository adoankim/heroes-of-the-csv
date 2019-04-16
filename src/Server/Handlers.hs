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
import Control.Monad.Trans.Reader

import Servant.Server
import Servant.API
import Heroe.Types
import Server.Types
import CSVHeroe

heroesScore :: MonadIO m => ReaderT String m (Handler [Heroe])
heroesScore = do
  filePath <- ask
  heroes <- liftIO $ getHeroesFromFile filePath
  pure . pure $  V.toList heroes

heroeHandlers :: MonadIO m => ReaderT String m (Server HeroeAPI)
heroeHandlers = heroesScore
