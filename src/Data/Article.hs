--
-- Data: Article
--

{-# LANGUAGE DeriveGeneric #-}

{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Data.Article 
  ( Article (..)
  , ArticleIndex (..)
  ) where


import Control.Monad.Except
  ( ExceptT
  , liftEither
  )
import Data.Aeson (ToJSON, FromJSON, toEncoding)
import Data.Map (Map)
import qualified Data.Map as M (fromList)
import Data.Text (Text)
import qualified Data.Text as T (pack)
import Data.Yaml
  ( decodeFileEither
  )
import GHC.Generics



data Article = Article {
    articleId :: ArticleId
  , title     :: Text
  , summary   :: Text
  , part      :: Int
  , series    :: Text
} deriving (Eq, Generic, Show)

instance ToJSON Article where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Article

newtype ArticleId = ArticleId {
  getArticleId :: Text
} deriving (Eq, Generic, Show)


data Series = Series {
    seriesId :: SeriesId
  , title    :: Text
} deriving (Eq, Generic, Show)

instance ToJSON Series where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Series

newtype SeriesId = SeriesId {
  getSeriesId :: Text
} deriving (Eq, Generic, Show)


data IndexFile = IndexFile {
    articles :: [Article]
  , series   :: [Series]
} deriving (Eq, Generic, Show)

instance ToJSON IndexFile where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON IndexFile


data ArticleIndex = ArticleIndex {
    byId       :: Map Text Article
  , bySeriesId :: Map Text [Article]
  , articles   :: [Article]
  , series     :: [Series]
}


articleIndexFromIndexFile :: IndexFile -> ArticleIndex
articleIndexFromIndexFile (IndexFile articles series) = ArticleIndex {
  --, bySeriesId = M.fromList $ (\s -> )
    articles   = articles
  , series     = series
  , byId       = M.fromList $ (\a -> (a.id, a)) <$> articles
  , bySeriesId = M.fromListWith (++) $ fmap $ (\s -> (s.id, s)) <$> (series)
}
  


-- | Reads all of the file icons in the directory into the index as Text
-- TODO check if is absolute or relative path, currently assumes relative
loadArticleIndex :: FilePath -> ExceptT LoadIndexFileError IO ArticleIndex 
loadArticleIndex indexPath = do
    eIndexFile <- liftEither $ T.decodeFileEither indexPath
    case eIndexFile of
      Left ex -> return $ LoadIndexFileError $ T.pack $ show ex
      Rigth ex -> do
        



data LoadIndexFileError = LoadIndexFileError Text
