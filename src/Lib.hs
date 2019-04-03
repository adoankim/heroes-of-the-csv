{-# LANGUAGE OverloadedStrings #-}

module Lib where
import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import Data.Csv
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


type MaybeHeroesVector = Maybe (V.Vector Name, V.Vector Heroe)

byteStringToHeroesVector :: BL.ByteString -> IO MaybeHeroesVector
byteStringToHeroesVector csvData = case decodeByName csvData of
  Left err -> pure Nothing
  Right (h, v) -> pure $ Just (h, v)

heroesVectorToByteString :: MaybeHeroesVector -> Maybe BL.ByteString
heroesVectorToByteString heroes = do
  h <- heroes
  pure $ encodeByName (fst h) $ V.toList $ snd h


sortHeroesById :: MaybeHeroesVector -> MaybeHeroesVector
sortHeroesById heroes = do
  h <- heroes
  pure $ (,) (fst h) $ V.fromList $ L.sortOn (_id) $ V.toList $ snd h

levelUpHeroes :: MaybeHeroesVector -> MaybeHeroesVector
levelUpHeroes heroes = do
    h <- heroes
    pure $ (,) (fst h) $ V.map levelUp $ snd h

allNamesInLowerCase :: MaybeHeroesVector -> MaybeHeroesVector
allNamesInLowerCase heroes = do
    h <- heroes
    pure $ (,) (fst h) $ V.map toLowerName $ snd h

printHeroesVector :: MaybeHeroesVector -> IO ()
printHeroesVector heroesDataVector = do
  let Just(_, heroes) = heroesDataVector
  V.forM_ heroes $ \heroe -> putStrLn $ (show $ name heroe) ++ " has " ++ (show $ points heroe) ++ " points!"

writeHeroesVector :: String -> MaybeHeroesVector -> IO ()
writeHeroesVector csvFilePath heroesDataVector = do
  let Just(heroesByteString) = heroesVectorToByteString heroesDataVector
  BL.writeFile csvFilePath heroesByteString

readCSVFile :: String -> Bool -> IO ()
readCSVFile csvFilePath dryRun = do
  csvData <- BL.readFile csvFilePath
  heroesVector <-  byteStringToHeroesVector csvData

  let transformedHeroesVector = sortHeroesById
        $ levelUpHeroes
        $ allNamesInLowerCase heroesVector

  if dryRun then
    printHeroesVector transformedHeroesVector
  else
    writeHeroesVector csvFilePath transformedHeroesVector

