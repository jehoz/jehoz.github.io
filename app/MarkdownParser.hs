{-# LANGUAGE OverloadedStrings #-}

module MarkdownParser where

import Data.Char (isAlphaNum, isPrint, isSpace)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

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

type Parser = Parsec Void Text

pDocument :: Parser Document
pDocument = Document <$> some pBlock

pBlock :: Parser Block
pBlock = space >> choice [BParagraph <$> pParagraph]

pParagraph :: Parser Paragraph
pParagraph = try $ do
  inlines <- some pInline
  _ <- eol >> hspace >> eol >> space
  return $ Paragraph inlines

pInline :: Parser Inline
pInline =
  choice
    [ pLineBreak,
      pSoftBreak,
      pSpace,
      pPlain
    ]

pLineBreak :: Parser Inline
pLineBreak =
  let spaces = try $ string "  " >> hspace >> eol
      bslash = try $ char '\\' >> eol
   in (spaces <|> bslash) >> return LineBreak

pSoftBreak :: Parser Inline
pSoftBreak = try $ hspace >> eol >> hspace >> notFollowedBy eol >> return Space

pSpace :: Parser Inline
pSpace = try $ hspace1 >> return Space

pPlain :: Parser Inline
pPlain = Plain <$> takeWhile1P (Just "alphanumeric") (\c -> isPrint c && not (isSpace c))
