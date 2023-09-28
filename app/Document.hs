{-# LANGUAGE OverloadedStrings #-}

module Document where

import Control.Monad (forM_)
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
  | Emphasis Text
  | Strong Text

--------------------
-- ToMarkup intances
--------------------

instance ToMarkup Document where
  toMarkup (Document bs) = docTypeHtml $ Html.body $ forM_ bs toMarkup

instance ToMarkup Block where
  toMarkup block = case block of
    Paragraph inlines -> Html.p $ forM_ inlines toMarkup

instance ToMarkup Inline where
  toMarkup inline = case inline of
    Space -> " "
    LineBreak -> Html.br
    Plain text -> toMarkup text
    Emphasis text -> toMarkup text
    Strong text -> toMarkup text
