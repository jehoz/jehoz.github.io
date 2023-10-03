{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Website where

import CMark (commonmarkToNode)
import Control.Monad (forM)
import Data.Bifunctor (Bifunctor (..))
import Data.List (partition)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TE
import Data.Time (Day, defaultTimeLocale, parseTimeM)
import Data.YAML
import FileUtils (getRelativePathsInside)
import MarkdownNode
import System.Exit (die)
import System.FilePath (isExtensionOf, (</>))

data Website = Website
  { websiteRootDir :: FilePath,
    websiteArticles :: [Article],
    websiteStaticFiles :: [FilePath]
  }

data Article = Article ArticleProps MarkdownNode

data ArticleProps = ArticleProps
  { articlePath :: FilePath,
    articleTitle :: Text,
    articleDate :: Day,
    articleTags :: [Tag]
  }

type Tag = Text

instance FromYAML ArticleProps where
  parseYAML = withMap "Article" $ \m -> do
    title <- m .: "title"
    date <- m .: "date"
    tags <- fromMaybe [] <$> m .:? "tags"
    return $ ArticleProps "" title date tags

instance FromYAML Day where
  parseYAML = withStr "Day" $ \s ->
    case parseTimeM True defaultTimeLocale "%Y-%-m-%-d" (T.unpack s) of
      Just day -> return day
      Nothing -> fail "Malformed date value, should be YYYY-MM-DD"

loadConent :: FilePath -> IO Website
loadConent contentDir = do
  relPaths <- getRelativePathsInside contentDir

  let (mdFiles, otherFiles) = partition (".md" `isExtensionOf`) relPaths

  articles <- forM mdFiles $ \rpath -> do
    mdText <- TIO.readFile (contentDir </> rpath)
    case parseArticle mdText of
      Left err -> die (T.unpack err)
      Right (Article props nodes) ->
        return $ Article (props {articlePath = rpath}) nodes

  return $ Website contentDir articles otherFiles

parseArticle :: Text -> Either Text Article
parseArticle text = do
  (firstLine, restLines) <- case T.lines text of
    [] -> Left "Cannot parse empty file"
    (l : ls) -> Right (l, ls)

  (frontMatter, markdown) <- case T.strip firstLine of
    "___" -> Right $ bimap T.unlines (T.unlines . tail) $ span ((/= "---") . T.strip) restLines
    _ -> Left $ "Expected front matter, found \"" <> firstLine <> "\" instead"

  props <- case decode1 (TE.encodeUtf8 (TL.fromStrict frontMatter)) of
    Left (_, e) -> Left (T.pack e)
    Right p -> Right p

  Right $ Article props (MarkdownNode $ commonmarkToNode [] markdown)
