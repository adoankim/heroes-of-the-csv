{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module CSVHeroe.Types where

import Data.Csv
import qualified Data.Vector as V

import Heroe.Types

type HeroesVector = (V.Vector Name, V.Vector Heroe)

instance FromNamedRecord Heroe where
  parseNamedRecord r = Heroe <$> r .: "id" <*> r .: "name" <*> r .: "points"

instance ToNamedRecord Heroe where
  toNamedRecord (Heroe _id name points) = namedRecord [ "id" .= _id, "name" .= name, "points" .= points ]
