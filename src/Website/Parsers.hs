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
parsePage :: Text -> Either Text Page
parsePage text = do
  (firstLine, restLines) <- case T.lines text of
    [] -> Left "Cannot parse empty file (this error should never happen)"
    (l : ls) -> Right (l, ls)

  let (frontMatter, markdown) =
        case T.strip firstLine of
          "---" ->
            bimap T.unlines (T.unlines . tail) $
              span ((/= "---") . T.strip) restLines
          _ -> ("", T.unlines $ firstLine : restLines)

  let frontMatter' = TE.encodeUtf8 (TL.fromStrict frontMatter)
  attrs <- case decode1 frontMatter' of
    Left (pos, e) -> Left . T.pack $ prettyPosWithSource pos frontMatter' " error" <> e
    Right a -> Right a

  Right $
    Page
      { pageSourcePath = "",
        pageAttrs = attrs,
        pageContent = MarkdownNode (commonmarkToNode [] markdown)
      }

-- | A simple wrapper around `commonmarkToNode` that returns the `Node` wrapped
-- in the `MarkdownNode` newtype.
parseMarkdownNode :: Text -> MarkdownNode
parseMarkdownNode = MarkdownNode . commonmarkToNode []
