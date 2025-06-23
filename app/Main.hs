{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Applicative
import Website

data Options = Options
  { optContentDir :: FilePath,
    optTemplatesDir :: FilePath,
    optOutputDir :: FilePath,
    optSymlinkMode :: Bool
  }

options :: Parser Options
options =
  Options
    <$> strOption
      ( long "content-dir"
          <> short 'c'
          <> metavar "DIR"
          <> value "content"
          <> help "Directory containing Markdown pages and other static assets"
      )
    <*> strOption
      ( long "templates-dir"
          <> short 't'
          <> metavar "DIR"
          <> value "templates"
          <> help "Directory containing HTML templates"
      )
    <*> strOption
      ( long "output-dir"
          <> short 'o'
          <> metavar "DIR"
          <> value "output"
          <> help "Directory into which all the final static site is written"
      )
    <*> switch
      ( long "symlink-mode"
          <> short 's'
          <> help "Symlink static files in output directory instead of copying them"
      )

optInfo :: ParserInfo Options
optInfo = info (helper <*> options) fullDesc

main :: IO ()
main = do
  Options contentDir templatesDir outputDir symlinkMode <- execParser optInfo

  generateStaticSite $ do
    loadContent contentDir
    loadTemplates templatesDir
    if symlinkMode
      then renderToWithSymlinks outputDir
      else renderTo outputDir
