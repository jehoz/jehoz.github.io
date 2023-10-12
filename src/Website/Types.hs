{-# LANGUAGE OverloadedStrings #-}

-- | Module: Types
--
-- Defines all of the types used in the intermediate representation of the
-- website.
module Website.Types where

import CMark (ListAttributes (..), ListType (..), Node (Node), NodeType (..))
import Control.Applicative (asum)
import Data.Map (Map)
import Data.Text (Text)
import Data.YAML (FromYAML (parseYAML))
import Text.Blaze.Html5 (ToMarkup (toMarkup), ToValue (..), textValue, (!))
import qualified Text.Blaze.Html5 as Html
import qualified Text.Blaze.Html5.Attributes as Attr

-- | The encapsulation of all of the content within a website
data Website = Website
  { -- | Directory containing all website content
    websiteRootDir :: FilePath,
    -- | List of pages parsed from markdown files found in root directory
    websitePages :: [Page],
    -- | Relative paths to all non-markdown files found in root directory
    websiteStaticFiles :: [FilePath]
  }

-- | A page of content on the website
data Page = Page
  { -- | Path to the original markdown file
    pageSourcePath :: FilePath,
    -- | Map of attributes taken from front matter
    pageAttrs :: Map Text PageAttr,
    -- | Markdown AST parsed from source file
    pageContent :: MarkdownNode
  }

-- | Supported types of attributes defined in a page's front matter
data PageAttr
  = PABool Bool
  | PAInt Integer
  | PAText Text
  | PAList [PageAttr]
  | PAMap (Map Text PageAttr)

instance FromYAML PageAttr where
  parseYAML y =
    asum
      [ parseAs PABool y,
        parseAs PAInt y,
        parseAs PAText y,
        parseAs PAList y,
        parseAs PAMap y
      ]
    where
      parseAs f x = f <$> parseYAML x

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
