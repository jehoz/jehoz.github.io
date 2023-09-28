{-# LANGUAGE OverloadedStrings #-}

module MarkdownParser where

import Control.Monad (MonadPlus)
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
      pCodeSpan,
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
  let stars = try $ string "**" `enclosingSome` pInline
      unders = try $ string "__" `enclosingSome` pInline
   in Strong <$> (stars <|> unders)

pEmphasis :: Parser Inline
pEmphasis =
  let stars = try $ string "*" `enclosingSome` pInline
      unders = try $ string "_" `enclosingSome` pInline
   in Emphasis <$> (stars <|> unders)

pCodeSpan :: Parser Inline
pCodeSpan = try $ do
  backticks <- pack <$> some (char '`')
  CodeSpan <$> manyTill (pSoftBreak <|> pSpace <|> pPlain) (string backticks)

pPlain :: Parser Inline
pPlain =
  let alphaNum = try $ pack <$> some alphaNumChar
      misc = try $ singleton <$> printChar
   in Plain <$> (alphaNum <|> misc)

-------------------
-- Helper functions
-------------------

enclosingMany :: MonadPlus m => m end -> m a -> m [a]
enclosingMany bound p = bound >> manyTill p bound

enclosingSome :: MonadPlus m => m end -> m a -> m [a]
enclosingSome bound p = bound >> someTill p bound
