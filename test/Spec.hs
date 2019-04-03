{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
import           Test.Hspec.Core.Spec
import           Test.Hspec
import           Test.QuickCheck
import           Data.Text (Text)
import qualified          Data.Text as T

import Lib

main :: IO ()
main = hspec spec

spec :: SpecWith ()
spec = do csvTests


csvTests :: SpecWith ()
csvTests = describe
  "CSV tests"
  (do describe "Heroe" heroe)



instance Arbitrary Text where
  arbitrary = do
    text2 <- arbitrary
    pure (T.pack text2)

instance Arbitrary Heroe where
  arbitrary = do
    _id <- arbitrary
    name <- arbitrary
    points <- arbitrary

    pure Heroe { _id = _id, name = name, points = points }

heroe :: SpecM () ()
heroe = do
    it "Creates a heroe" (property (\h -> Heroe { _id = _id h, name = name h, points = points h } == (h :: Heroe)))
    it "Increase the heroe level" (property (\h -> levelUp h == Heroe { _id = _id h, name = name h, points = (points h) + 1 }))
    it "Lower case the heroe name" (property (\h -> name (toLowerName h) == T.toLower (name h)))
