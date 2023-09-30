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
      pInlineLink,
      pAutoLink,
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

pUri :: Parser Uri
pUri =
  let uriChar =
        choice
          [ alphaNumChar,
            oneOf (";,/?:@&=+$-_.!~*'#" :: [Char]),
            char '\\' >> oneOf ("()" :: [Char])
          ]
   in try $ some uriChar

pInlineLink :: Parser Inline
pInlineLink = try $ do
  let simpleInline = choice [pLineBreak, pSoftBreak, pSpace, pStrong, pEmphasis, pCodeSpan, pPlain]
  display <- someBetween (char '[') (char ']') simpleInline
  uri <- between (char '(') (char ')') pUri
  title <- optional $ someBetween (char '"') (char '"') printChar
  return $ InlineLink display uri title

pAutoLink :: Parser Inline
pAutoLink = AutoLink <$> someBetween (char '<') (char '>') printChar

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

manyBetween :: MonadPlus m => m start -> m end -> m a -> m [a]
manyBetween start end p = start >> manyTill p end

someBetween :: MonadPlus m => m start -> m end -> m a -> m [a]
someBetween start end p = start >> someTill p end

enclosingMany :: MonadPlus m => m end -> m a -> m [a]
enclosingMany bound = manyBetween bound bound

enclosingSome :: MonadPlus m => m end -> m a -> m [a]
enclosingSome bound = someBetween bound bound
