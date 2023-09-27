module Document where

import Data.Text (Text)

newtype Document = Document [Block]

data Block
  = BParagraph Paragraph

newtype Paragraph = Paragraph [Inline]

data Inline
  = Space
  | LineBreak
  | Plain Text
  | Emphasis Text
  | Strong Text
