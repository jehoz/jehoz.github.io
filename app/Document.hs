{-# LANGUAGE OverloadedStrings #-}

module Document where

import Data.Text.Lazy (Text)
import Text.Blaze.Html5 (ToMarkup (..), docTypeHtml, (!))
import qualified Text.Blaze.Html5 as Html
import qualified Text.Blaze.Html5.Attributes as Attr

newtype Document = Document [Block]

data Block
  = Paragraph [Inline]

type Uri = String

data Inline
  = Space
  | LineBreak
  | Plain Text
  | Emphasis [Inline]
  | Strong [Inline]
  | CodeSpan [Inline]
  | InlineLink [Inline] Uri (Maybe String)
  | AutoLink Uri

--------------------
-- ToMarkup intances
--------------------

instance ToMarkup Document where
  toMarkup (Document bs) = docTypeHtml $ Html.body $ mapM_ toMarkup bs

instance ToMarkup Block where
  toMarkup block = case block of
    Paragraph inlines -> Html.p $ mapM_ toMarkup inlines

instance ToMarkup Inline where
  toMarkup inline = case inline of
    Space -> " "
    LineBreak -> Html.br
    Plain text -> toMarkup text
    Emphasis inlines -> Html.em $ mapM_ toMarkup inlines
    Strong inlines -> Html.strong $ mapM_ toMarkup inlines
    CodeSpan inlines -> Html.code $ mapM_ toMarkup inlines
    InlineLink text uri title ->
      let uri' = Html.stringValue uri
          title' = Html.stringValue <$> title
       in case title' of
            Just t -> Html.a ! Attr.href uri' ! Attr.title t $ mapM_ toMarkup text
            Nothing -> Html.a ! Attr.href uri' $ mapM_ toMarkup text
    AutoLink uri -> Html.a ! Attr.href (Html.stringValue uri) $ toMarkup uri
