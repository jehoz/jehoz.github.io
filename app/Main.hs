{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Applicative
import Website (buildWebsite, readContent)

data Options = Options
  { optContentDir :: FilePath,
    optOutputDir :: FilePath
  }

options :: Parser Options
options =
  Options
    <$> strOption
      ( long "content-dir"
          <> short 'c'
          <> metavar "DIR"
          <> value "content"
          <> help
            ( "Uses everything under this directory to generate the website. "
                <> "All markdown files converted to HTML.  Everything else copied as a static asset."
            )
      )
    <*> strOption
      ( long "output-dir"
          <> short 'o'
          <> metavar "DIR"
          <> value "output"
          <> help "All site assets and generated HTML pages are placed in DIR"
      )

optInfo :: ParserInfo Options
optInfo = info (helper <*> options) fullDesc

main :: IO ()
main = do
  opts <- execParser optInfo

  let contentDir = optContentDir opts
      outputDir = optOutputDir opts

  putStrLn "Reading website content..."
  site <- readContent contentDir

  putStrLn "Building website..."
  buildWebsite site outputDir
