{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (filterM, zipWithM_)
import Data.List (nub, partition)
import qualified Data.Text.IO as T
import Data.Text.Lazy (toStrict)
import MarkdownNode
import Options.Applicative
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (isExtensionOf, takeDirectory, (-<.>), (</>))
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 (toHtml)

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
getRelativePathsInside :: FilePath -> IO [FilePath]
getRelativePathsInside parent = do
  names <- listDirectory parent
  files <- filterM (doesFileExist . (parent </>)) names
  dirs <- filterM (doesDirectoryExist . (parent </>)) names
  (files ++) . concat <$> mapM recur dirs
  where
    recur subd = do
      ps <- getRelativePathsInside (parent </> subd)
      return $ (subd </>) <$> ps

-- | Parse a given Markdown file at first path.  Use it to generate an HTML
-- file saved to second path.
generateHtmlFile :: FilePath -> FilePath -> IO ()
generateHtmlFile mdPath htmlPath = do
  mdText <- T.readFile mdPath
  let nodes = parseMarkdownNode mdText
  let htmlText = renderHtml $ toHtml nodes
  T.writeFile htmlPath (toStrict htmlText)

printNamesThen :: (FilePath -> FilePath -> IO ()) -> FilePath -> FilePath -> IO ()
printNamesThen func f1 f2 = do
  putStrLn $ concat ["\t", f1, " --> ", f2]
  func f1 f2

main :: IO ()
main = do
  opts <- execParser optInfo

  let contentDir = optContentDir opts
  let outputDir = optOutputDir opts

  relPaths <- getRelativePathsInside contentDir

  let (mdFiles, otherFiles) = partition (".md" `isExtensionOf`) relPaths

  -- helper op to prepend directory to all paths in list
  let (</$>) d = fmap (d </>)

  -- create folders in output directory
  let subdirs = (outputDir </$>) . nub $ takeDirectory <$> relPaths
  mapM_ (createDirectoryIfMissing True) subdirs

  -- convert markdowm files to html
  putStrLn "Converting Markdown files to HTML"
  zipWithM_
    (printNamesThen generateHtmlFile)
    (contentDir </$> mdFiles)
    (outputDir </$> ((-<.> "html") <$> mdFiles))

  -- copy all non-markdown files
  putStrLn "Copying rest of files"
  zipWithM_
    (printNamesThen copyFile)
    (contentDir </$> otherFiles)
    (outputDir </$> otherFiles)
