{-# LANGUAGE OverloadedStrings #-}

module Document where

import Data.Text.Lazy (Text)
import Text.Blaze.Html5 (ToMarkup (..), docTypeHtml)
import qualified Text.Blaze.Html5 as Html

newtype Document = Document [Block]

data Block
  = Paragraph [Inline]

data Inline
  = Space
  | LineBreak
  | Plain Text
  | Emphasis [Inline]
  | Strong [Inline]

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
