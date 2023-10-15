{-# LANGUAGE OverloadedRecordDot #-}
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
import System.FilePath ((</>))
import Text.Blaze.Html5 (ToMarkup (toMarkup), ToValue (..), textValue, (!))
import qualified Text.Blaze.Html5 as Html
import qualified Text.Blaze.Html5.Attributes as Attr
import Text.Mustache (Template, ToMustache (toMustache))

-- | The encapsulation of all of the content within a website
data Website = Website
  { -- | List of pages parsed from markdown files found in root directory
    pages :: [Page],
    -- | Relative paths to all non-markdown files found in root directory
    staticFiles :: [SplitPath],
    -- | Map of compiled Mustache templates found in templates directory
    templateMap :: Map Text Template,
    -- | List of directories where static site files will be written
    outputDirectories :: [FilePath]
  }

instance Semigroup Website where
  x <> y =
    Website
      (x.pages <> y.pages)
      (x.staticFiles <> y.staticFiles)
      (x.templateMap <> y.templateMap)
      (x.outputDirectories <> y.outputDirectories)

instance Monoid Website where
  mempty = Website mempty mempty mempty mempty

-- | A page of content on the website
data Page = Page
  { -- | Path to the original markdown file
    sourcePath :: SplitPath,
    -- | Map of attributes taken from front matter
    attrs :: Map Text PageAttr,
    -- | Markdown AST parsed from source file
    content :: Markdown
  }

-- | Supported types of attributes that can be defined in a page's front matter
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

instance ToMustache PageAttr where
  toMustache (PABool b) = toMustache b
  toMustache (PAInt i) = toMustache i
  toMustache (PAText t) = toMustache t
  toMustache (PAList l) = toMustache l
  toMustache (PAMap m) = toMustache m

-- | Just a wrapper around the `Node` type from the `cmark` library.
newtype Markdown = Markdown {getNode :: Node}

replaceNode :: Markdown -> NodeType -> NodeType -> Markdown
replaceNode (Markdown node) from to = Markdown (recur node)
  where
    recur (Node pos ntype children) =
      let ntype' = if ntype == from then to else ntype
       in Node pos ntype' (recur <$> children)

instance ToMarkup Markdown where
  toMarkup (Markdown (Node _ nodeType children)) =
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
      iter = mapM_ (toMarkup . Markdown)

      -- slightly lazy and hacky way to implement tight lists: just replace any
      -- paragraph in children with custom block without tags
      tighten n =
        let (Markdown n') = replaceNode (Markdown n) PARAGRAPH (CUSTOM_BLOCK "" "")
         in n'

      getHeading i = [Html.h1, Html.h2, Html.h3, Html.h4, Html.h5, Html.h6] !! (i - 1)

-- | The path to a file that is split somewhere in the middle denote a relative
-- path within a parent directory.
-- Useful for managing files from multiple content or templates directories
type SplitPath = (FilePath, FilePath)

fullPath :: SplitPath -> FilePath
fullPath = uncurry (</>)

parentDirectory :: SplitPath -> FilePath
parentDirectory = fst

relativePath :: SplitPath -> FilePath
relativePath = snd
