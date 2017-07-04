{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

-- Dokumentation:
-- Data.Yaml https://hackage.haskell.org/package/yaml-0.8.23.1/docs/Data-Yaml.html
-- Path https://hackage.haskell.org/package/path-0.6.0/docs/Path.html
-- Path IO https://hackage.haskell.org/package/path-io-1.3.1/docs/Path-IO.html
-- Codec.Archive.Zip https://hackage.haskell.org/package/zip-0.1.11/docs/Codec-Archive-Zip.html#t:EntrySelector
-- CMark (Markdownparsing) https://hackage.haskell.org/package/cmark-0.5.5.1/docs/CMark.html


module Main where

import CMark (Url, Node(..), NodeType(..))
import qualified CMark as MD
import qualified Codec.Archive.Zip as Zip
import Control.Monad (mapM, mapM_, (>=>))
import qualified Data.Aeson as Aeson
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text.Encoding as Enc
import Data.Yaml (ToJSON(..), FromJSON(..))
import qualified Data.Yaml as Yaml
import GHC.Generics
import Path (Path, Rel, Abs, File)
import qualified Path as P
import qualified Path.IO as PIO

-- Zipt alle Verzeichnisse und Unterdatein

main :: IO ()
main = do
  path <- PIO.getCurrentDir
  target <- P.parseRelFile "Zip.zip"

  putStrLn "looking for content "
  
  putStrLn =<< maybe "not found" show <$> getZipDescription target
  putStrLn =<< show <$> Zip.withArchive target allMarkdownUris
  
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


allMarkdownUris :: Zip.ZipArchive [Url]
allMarkdownUris = do
  selectors <- findArchiveFiles ".md"
  concat <$> mapM (fmap extractImageUris . getMarkdownContent) selectors

extractImageUris :: Text -> [Url]
extractImageUris =
  collectUris . MD.commonmarkToNode []
  where
    collectUris :: Node -> [Url]
    collectUris (Node _ (IMAGE url _) children) = url : concatMap collectUris children
    collectUris (Node _ _ children)             = concatMap collectUris children

  
getMarkdownContent :: Zip.EntrySelector -> Zip.ZipArchive Text
getMarkdownContent selector = Enc.decodeUtf8 <$> Zip.getEntry selector



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
