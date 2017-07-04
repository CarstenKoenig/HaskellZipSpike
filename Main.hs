{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

-- Dokumentation:
-- Data.Yaml https://hackage.haskell.org/package/yaml-0.8.23.1/docs/Data-Yaml.html
-- Path https://hackage.haskell.org/package/path-0.6.0/docs/Path.html
-- Path IO https://hackage.haskell.org/package/path-io-1.3.1/docs/Path-IO.html
-- Codec.Archive.Zip https://hackage.haskell.org/package/zip-0.1.11/docs/Codec-Archive-Zip.html#t:EntrySelector


module Main where

import qualified Codec.Archive.Zip as Zip
import Control.Monad (mapM_, (>=>))
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import Data.Yaml (ToJSON(..), FromJSON(..))
import qualified Data.Yaml as Yaml
import GHC.Generics
import Path (Path, Rel, Abs, File)
import qualified Path as P
import qualified Path.IO as PIO
import Data.Map (Map)
import qualified Data.Map as Map

-- Zipt alle Verzeichnisse und Unterdatein

main :: IO ()
main = do
  path <- PIO.getCurrentDir
  target <- P.parseRelFile "Zip.zip"

  putStrLn "looking for content "
  
  putStrLn =<< maybe "not found" show <$> getZipDescription target
  putStrLn "done"


zipCurrentFolder :: IO ()
zipCurrentFolder = do  
  path <- PIO.getCurrentDir
  files <- snd <$> PIO.listDirRecur path
  target <- P.parseRelFile "Zip.zip"
  let selectFile = PIO.makeRelative path >=> Zip.mkEntrySelector
  Zip.createArchive target $ mapM_ (Zip.loadEntry Zip.BZip2 selectFile) files


getZipDescription :: Path Rel File -> IO (Maybe BlogEntryDescription)
getZipDescription archivePath = do
  Zip.withArchive archivePath $ do
    yamls <- findArchiveFiles ".yaml"
    case yamls of
      []           -> return Nothing
      selector : _ -> Yaml.decode <$> Zip.getEntry selector
  

findArchiveFiles :: String -> Zip.ZipArchive [Zip.EntrySelector]
findArchiveFiles  extension = do
  Map.keys . Map.filterWithKey hasExtension <$> Zip.getEntries
  where
    hasExtension sel _ = P.fileExtension (Zip.unEntrySelector sel) == extension
    
  

data BlogEntryDescription =
  BlogEntryDescription
  { title :: Text
  , categories :: [Category]
  } deriving (Generic, Show)


newtype Category
  = Category String
  deriving (Generic, Show)


instance ToJSON BlogEntryDescription where
  toJSON = Aeson.genericToJSON Aeson.defaultOptions
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions


instance ToJSON Category where
  toJSON = Aeson.genericToJSON Aeson.defaultOptions
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions


instance FromJSON BlogEntryDescription where
  parseJSON = Aeson.genericParseJSON Aeson.defaultOptions


instance FromJSON Category where
  parseJSON = Aeson.genericParseJSON Aeson.defaultOptions
