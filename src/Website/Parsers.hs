{-# LANGUAGE OverloadedStrings #-}

-- | Module: Website.Parsers where
--
-- Functions for parsing website-related types from raw text input
module Website.Parsers where

import CMark (commonmarkToNode)
import Data.Bifunctor (Bifunctor (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TE
import Data.YAML (decode1, prettyPosWithSource)
import Website.Types

-- | Attempt to parse the content of a markdown file into an `Article`.
-- Expects the file to begin with font-matter like so:
--
-- > ---
-- > title: Some article title
-- > date: 2001-9-11
-- > ---
-- > The actual markdown goes down here
-- > ...
--
-- Where everything between the @---@ lines is parsed as a YAML defining some
-- metadata values (title and date which are mandatory, along with an optional
-- list of tags).  Everything underneath the front matter is treated as the
-- markdown-formatted content of the article.
parseArticle :: Text -> Either Text Article
parseArticle text = do
  (firstLine, restLines) <- case T.lines text of
    [] -> Left "Cannot parse empty file"
    (l : ls) -> Right (l, ls)

  (frontMatter, markdown) <- case T.strip firstLine of
    "---" ->
      Right $
        bimap T.unlines (T.unlines . tail) $
          span ((/= "---") . T.strip) restLines
    _ -> Left $ "Expected front matter, found \"" <> firstLine <> "\" instead"

  let encodedText = TE.encodeUtf8 (TL.fromStrict frontMatter)
  props <- case decode1 encodedText of
    Left (pos, e) -> Left . T.pack $ prettyPosWithSource pos encodedText " error" <> e
    Right prop -> Right prop

  Right $ Article props (MarkdownNode $ commonmarkToNode [] markdown)

-- | A simple wrapper around `commonmarkToNode` that returns the `Node` wrapped
-- in the `MarkdownNode` newtype.
parseMarkdownNode :: Text -> MarkdownNode
parseMarkdownNode = MarkdownNode . commonmarkToNode []
