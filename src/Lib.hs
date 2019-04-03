{-# LANGUAGE OverloadedStrings #-}

module Lib where
import           Data.Bifunctor
import           Control.Applicative
import qualified Data.ByteString.Lazy as BL
import           Data.Csv
import qualified Data.Vector as V
import qualified Data.List as L
import           Data.Text (Text)
import qualified Data.Text as T

data Heroe = Heroe
  {
      _id     :: !Int
    , name   :: !Text
    , points :: !Int
  } deriving(Show)

instance Eq Heroe where
  (==) (Heroe _id1 name1 points1) (Heroe _id2 name2 points2) = _id1 == _id2 && name1 == name2 && points1 == points2

instance FromNamedRecord Heroe where
    parseNamedRecord r = Heroe <$> r .: "id" <*> r .: "name" <*> r .: "points"

instance ToNamedRecord Heroe where
    toNamedRecord (Heroe _id name points) = namedRecord [ "id" .= _id, "name" .= name, "points" .= points ]

levelUp :: Heroe -> Heroe
levelUp heroe = heroe { points = (+1) $ points heroe  }

toLowerName :: Heroe -> Heroe
toLowerName heroe = heroe { name = T.toLower $ name heroe }

type HeroesVector = (V.Vector Name, V.Vector Heroe)

byteStringToHeroesVector :: BL.ByteString -> IO (Maybe HeroesVector)
byteStringToHeroesVector csvData = case decodeByName csvData of
  Left err -> pure Nothing
  Right (h, v) -> pure $ Just (h, v)

heroesVectorToByteString :: HeroesVector -> BL.ByteString
heroesVectorToByteString (header, rows) = encodeByName header (V.toList rows)

sortHeroesById :: V.Vector Heroe -> V.Vector Heroe
sortHeroesById = V.fromList . L.sortOn _id . V.toList

levelUpHeroes :: V.Vector Heroe -> V.Vector Heroe
levelUpHeroes = V.map levelUp

allNamesInLowerCase :: V.Vector Heroe -> V.Vector Heroe
allNamesInLowerCase = V.map toLowerName

printHeroesVector :: HeroesVector -> IO ()
printHeroesVector (_, heroes) = do
  V.forM_ heroes $ \heroe -> putStrLn $ (show $ name heroe) ++ " has " ++ (show $ points heroe) ++ " points!"

writeHeroesVector :: String -> HeroesVector -> IO ()
writeHeroesVector csvFilePath heroesDataVector = do
  let heroesByteString = heroesVectorToByteString heroesDataVector
  BL.writeFile csvFilePath heroesByteString

readCSVFile :: String -> Bool -> IO ()
readCSVFile csvFilePath dryRun = do
  csvData <- BL.readFile csvFilePath
  maybeHeroesVector <- byteStringToHeroesVector csvData
  case maybeHeroesVector of
    Nothing -> putStrLn "Failed to parse CSV."
    Just file -> do
      let transformedHeroesVector (header, rows) =
            (header, (sortHeroesById . levelUpHeroes . allNamesInLowerCase) rows)
      if dryRun
        then printHeroesVector (transformedHeroesVector file)
        else writeHeroesVector csvFilePath (transformedHeroesVector file)
