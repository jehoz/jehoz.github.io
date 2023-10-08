{-# LANGUAGE OverloadedStrings #-}

module Website.Parsers where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TE
import qualified Data.Text.Lazy as TL
import Website.Types
import Data.Bifunctor (Bifunctor(..))
import Data.YAML (decode1, prettyPosWithSource)
import CMark (commonmarkToNode)

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

parseMarkdownNode :: Text -> MarkdownNode
parseMarkdownNode = MarkdownNode . commonmarkToNode []
