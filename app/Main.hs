{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Applicative
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Blaze.Html5 (Html, body, docTypeHtml, p)
import qualified Text.Blaze.Html5 as H

data Options = Options
  { optOutputDir :: FilePath
  }

options :: Parser Options
options =
  Options
    <$> strOption
      ( long "outdir"
          <> short 'o'
          <> metavar "DIR"
          <> value "."
          <> help "Generate HTML pages in DIR"
      )

optInfo :: ParserInfo Options
optInfo = info (helper <*> options) fullDesc

dummyPage :: Html
dummyPage = docTypeHtml $ do
  H.head $ do
    H.title "dummy page"
  body $ do
    p "dummy page"

main :: IO ()
main = do
  opts <- execParser optInfo
  let d = optOutputDir opts
  createDirectoryIfMissing True d
  writeFile (d </> "index.html") $ renderHtml dummyPage
