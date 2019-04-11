module Heroe.Types where

import GHC.Generics
import Data.Text

data Heroe = Heroe
  {
      _id     :: !Int
    , name   :: !Text
    , points :: !Int
  } deriving(Show)

instance Eq Heroe where
  (==) (Heroe _id1 name1 points1) (Heroe _id2 name2 points2) = _id1 == _id2 && name1 == name2 && points1 == points2
