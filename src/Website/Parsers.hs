{-# LANGUAGE OverloadedStrings #-}

-- | Module: Website.Parsers where
--
-- Functions for parsing website-related types from raw text input
module Website.Parsers where

import CMark (commonmarkToNode)
import Data.Bifunctor (Bifunctor (..))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TE
import Data.Time
import Data.YAML (decode1, prettyPosWithSource)
import Website.Types

-- | Attempt to parse the content of a markdown file into an `Page`.
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
-- metadata values.  Everything underneath the front matter is treated as the
-- markdown-formatted content of the page.
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
          _ -> ("", text)

  let frontMatter' = TE.encodeUtf8 (TL.fromStrict frontMatter)
  attrMap <- case decode1 frontMatter' of
    Left (pos, e) -> Left . T.pack $ prettyPosWithSource pos frontMatter' " error" <> e
    Right a -> Right (addSpecialAttrs a)

  Right $
    Page
      { sourcePath = ("", ""),
        attrs = attrMap,
        content = Markdown (commonmarkToNode [] markdown)
      }

-- | A simple wrapper around `commonmarkToNode` that returns the `Node` wrapped
-- in the `Markdown` newtype.
parseMarkdownNode :: Text -> Markdown
parseMarkdownNode = Markdown . commonmarkToNode []

-- | Adds special auto-generated attributes to the parsed front-matter
-- Curently only special attribute is `date-pretty`
addSpecialAttrs :: Map Text PageAttr -> Map Text PageAttr
addSpecialAttrs attrMap = case M.lookup "date" attrMap of
  Just (PAText date) -> M.insert "date-pretty" (PAText $ prettifyDate date) attrMap
  _ -> attrMap
  where
    prettifyDate d =
      let parsedDate = parseTimeOrError True defaultTimeLocale "%Y-%m-%d" (T.unpack d) :: UTCTime
       in T.pack $ formatTime defaultTimeLocale "%b %-d, %Y" parsedDate
