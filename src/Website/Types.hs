{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Module: Types
--
-- Defines all of the types used in the intermediate representation of the
-- website.
module Website.Types where

import CMark (ListAttributes (..), ListType (..), Node (Node), NodeType (..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (Day, defaultTimeLocale, parseTimeM)
import Data.YAML (FromYAML (parseYAML), withMap, withStr, (.:), (.:?))
import Text.Blaze.Html5 (ToMarkup (toMarkup), ToValue (..), textValue, (!))
import qualified Text.Blaze.Html5 as Html
import qualified Text.Blaze.Html5.Attributes as Attr

-- | The encapsulation of all of the content within a website
data Website = Website
  { -- | Directory containing all website content (.md articles and static files)
    websiteRootDir :: FilePath,
    -- | List of articles parsed from markdown files found in root oirectory
    websiteArticles :: [Article],
    -- | Relative paths to all non-markdown files found in root directory
    websiteStaticFiles :: [FilePath]
  }

-- | An article on the website.  Contains some metadata and the markdown node
-- tree parsed from the source .md file.
-- (See the documentation for `parseArticle` for the expected format of the
-- input file).
data Article = Article ArticleProps MarkdownNode

-- | All of the metadata for a particular article (mostly taken from the
-- article's front-matter)
data ArticleProps = ArticleProps
  { -- | Path to original markdown file
    articlePath :: FilePath,
    -- | Title of article taken from front-matter
    articleTitle :: Text,
    -- | Date of article taken from front-matter
    articleDate :: Day,
    -- | Optional list of tags taken from front-matter
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

-- | Just a wrapper around the `Node` type from the `cmark` library.
newtype MarkdownNode = MarkdownNode {getNode :: Node}

instance ToMarkup MarkdownNode where
  toMarkup (MarkdownNode (Node _ nodeType children)) =
    case nodeType of
      DOCUMENT ->
        Html.docTypeHtml $
          Html.body $
            iter children
      THEMATIC_BREAK -> Html.hr
      PARAGRAPH ->
        Html.p $
          iter children
      BLOCK_QUOTE ->
        Html.blockquote $
          iter children
      HTML_BLOCK text -> do
        toMarkup text
        iter children
      CUSTOM_BLOCK enter exit -> do
        toMarkup enter
        iter children
        toMarkup exit
      CODE_BLOCK info text ->
        Html.pre $
          Html.code ! Attr.class_ (textValue $ "language-" <> info) $
            toMarkup text
      HEADING lvl ->
        getHeading lvl $
          iter children
      LIST lAttrs ->
        let htmlElem = case listType lAttrs of
              ORDERED_LIST -> Html.ol
              BULLET_LIST -> Html.ul
            children' = if listTight lAttrs then tighten <$> children else children
         in htmlElem ! Attr.start (toValue $ listStart lAttrs) $
              iter children'
      ITEM ->
        Html.li $
          iter children
      TEXT text -> toMarkup text
      SOFTBREAK -> toMarkup (" " :: String)
      LINEBREAK -> Html.br
      HTML_INLINE text -> do
        toMarkup text
        iter children
      CUSTOM_INLINE enter exit -> do
        toMarkup enter
        iter children
        toMarkup exit
      CODE text ->
        Html.code $
          toMarkup text
      EMPH ->
        Html.em $
          iter children
      STRONG ->
        Html.strong $
          iter children
      LINK url title ->
        Html.a ! Attr.href (toValue url) ! Attr.title (toValue title) $
          iter children
      IMAGE url title ->
        Html.img ! Attr.src (toValue url) ! Attr.title (toValue title)
    where
      iter = mapM_ (toMarkup . MarkdownNode)

      -- slightly lazy and hacky way to implement tight lists: just replace any
      -- paragraph in children with custom block without tags
      tighten (Node pos nt ch) = case nt of
        PARAGRAPH -> Node pos (CUSTOM_BLOCK "" "") (tighten <$> ch)
        _ -> Node pos nt (tighten <$> ch)

      getHeading i = [Html.h1, Html.h2, Html.h3, Html.h4, Html.h5, Html.h6] !! (i - 1)
