{-# LANGUAGE OverloadedStrings #-}

module MarkdownParser where

import Data.Text.Lazy (Text, pack, singleton)
import Data.Void (Void)
import Document
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text

pDocument :: Parser Document
pDocument = Document <$> some pBlock

pBlock :: Parser Block
pBlock = space >> choice [pParagraph]

pParagraph :: Parser Block
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
      pStrong,
      pEmphasis,
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

pStrong :: Parser Inline
pStrong =
  let stars = try $ string "**" >> manyTill pInline (string "**")
      unders = try $ string "__" >> manyTill pInline (string "__")
   in Strong <$> (stars <|> unders)

pEmphasis :: Parser Inline
pEmphasis =
  let stars = try $ string "*" >> manyTill pInline (string "*")
      unders = try $ string "_" >> manyTill pInline (string "_")
   in Emphasis <$> (stars <|> unders)

pPlain :: Parser Inline
pPlain =
  let alphaNum = try $ pack <$> some alphaNumChar
      misc = try $ singleton <$> printChar
   in Plain <$> (alphaNum <|> misc)
