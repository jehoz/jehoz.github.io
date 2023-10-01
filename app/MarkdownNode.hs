{-# LANGUAGE OverloadedStrings #-}

module MarkdownNode where

import CMark (ListAttributes (..), ListType (..), Node (..), NodeType (..), commonmarkToNode)
import Data.Text (Text)
import Text.Blaze.Html5 (ToMarkup (..), ToValue (toValue), textValue, (!))
import qualified Text.Blaze.Html5 as Html
import qualified Text.Blaze.Html5.Attributes as Attr

newtype MarkdownNode = MarkdownNode {getNode :: Node}

parseMarkdownNode :: Text -> MarkdownNode
parseMarkdownNode = MarkdownNode . commonmarkToNode []

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
