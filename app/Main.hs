{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (filterM, zipWithM_)
import Data.List (partition, nub)
import Data.Text.Lazy.IO (readFile, writeFile)
import MarkdownParser (pDocument)
import Options.Applicative
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (isExtensionOf, (-<.>), (</>), takeDirectory)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 (toHtml)
import Text.Megaparsec (errorBundlePretty, parse)
import Prelude hiding (readFile, writeFile)

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

-- | Get a list of relative paths to all files (not directories) inside the
-- given directory and all subdirectories.
getRelativeFilepaths :: FilePath -> IO [FilePath]
getRelativeFilepaths parent = do
  names <- listDirectory parent
  files <- filterM (doesFileExist . (parent </>)) names
  dirs <- filterM (doesDirectoryExist . (parent </>)) names
  (files ++) . concat <$> mapM recur dirs
  where
    recur subd = do
      ps <- getRelativeFilepaths (parent </> subd)
      return $ (subd </>) <$> ps

-- | Parse a given Markdown file at first path.  Use it to generate an HTML
-- file saved to second path.
convertMarkdownToHtml :: FilePath -> FilePath -> IO ()
convertMarkdownToHtml mdPath htmlPath = do
  putStrLn $ "Generating page: \"" ++ mdPath ++ "\" -> \"" ++ htmlPath ++ "\""
  mdText <- readFile mdPath
  case parse pDocument mdPath mdText of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right doc -> do
      writeFile htmlPath (renderHtml $ toHtml doc)

main :: IO ()
main = do
  opts <- execParser optInfo

  let cDir = optContentDir opts
  let oDir = optOutputDir opts

  relPaths <- getRelativeFilepaths cDir

  let (mdFiles, otherFiles) = partition (".md" `isExtensionOf`) relPaths

  -- helpers
  let srcDir = fmap (cDir </>)
  let dstDir = fmap (oDir </>)

  -- create folders in output directory
  let subdirs = dstDir . nub $ takeDirectory <$> relPaths
  mapM_ (createDirectoryIfMissing True) subdirs

  -- copy all non-markdown files
  zipWithM_ copyFile (srcDir otherFiles) (dstDir otherFiles)

  -- convert markdowm files to html
  zipWithM_ convertMarkdownToHtml (srcDir mdFiles) ((-<.> "html") <$> dstDir mdFiles)
