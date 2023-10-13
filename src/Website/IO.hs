{-# LANGUAGE OverloadedStrings #-}

-- | Module: Website.IO
--
-- Functions for reading and writing website content from/to the filesystem.
module Website.IO where

import Control.Monad (filterM, forM, forM_)
import Data.List (partition)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)
import System.Exit (die)
import System.FilePath (isExtensionOf, takeDirectory, (-<.>), (</>))
import Text.Blaze.Html (toHtml)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Mustache (Template (name), automaticCompile, substitute)
import Website.Parsers
import Website.Types

-- | Traverse through a directory, parse all @.md@ files as `Page`s and treat
-- the rest as static files.  Use all of this to populate a `Website`
loadFrom :: FilePath -> FilePath -> IO Website
loadFrom contentDir templatesDir = do
  -- load pages and static content
  contentPaths <- listAllFilesRelative contentDir
  let (mdFiles, otherFiles) = partition (".md" `isExtensionOf`) contentPaths

  pages <- forM mdFiles $ \rpath -> do
    mdText <- TIO.readFile (contentDir </> rpath)
    case parsePage mdText of
      Left err -> die ("Error parsing " <> rpath <> "\n" <> T.unpack err)
      Right page ->
        return $ page {pageSourcePath = rpath}

  -- load html page templates
  templatePaths <- filter (".html" `isExtensionOf`) <$> listAllFilesRelative templatesDir

  templates <-
    catMaybes
      <$> forM
        templatePaths
        ( \path -> do
            res <- automaticCompile [templatesDir] path
            case res of
              Left err -> print err >> return Nothing
              Right thing -> return (Just thing)
        )
  let kvPairs = (\t -> (T.pack $ name t, t)) <$> templates

  return $
    Website
      { websiteRootDir = contentDir,
        websitePages = pages,
        websiteStaticFiles = otherFiles,
        websiteTemplates = M.fromList kvPairs
      }

-- | Generate all of the files for a static site in the given directory.
-- This involves rendering each page as an html file, and simply copying the
-- static files from the content directory (keeping the original directory
-- structure)
renderTo :: FilePath -> Website -> IO ()
renderTo outputDir (Website sourceDir pages staticFiles templates) = do
  -- render html pages
  forM_ pages $ \p -> do
    let writePath = outputDir </> pageSourcePath p -<.> "html"
    createDirectoryIfMissing True (takeDirectory writePath)
    TIO.writeFile writePath (renderPage p)

  -- copy static files
  forM_ staticFiles $ \rpath -> do
    let fromPath = sourceDir </> rpath
        toPath = outputDir </> rpath
    createDirectoryIfMissing True (takeDirectory toPath)
    copyFile fromPath toPath
  where
    renderContent = TL.toStrict . renderHtml . toHtml . pageContent

    renderPage p = fromMaybe (renderContent p) $ do
      (PAText n) <- M.lookup "template" (pageAttrs p)
      template <- M.lookup n templates
      let attrs = M.insert "body" (PAText $ renderContent p) (pageAttrs p)
      return (substitute template attrs)

-- | Get a list of absolute paths to all files (not directories) inside the
-- given directory and all subdirectories.
listAllFiles :: FilePath -> IO [FilePath]
listAllFiles dir = do
  fs <- listAllFilesRelative dir
  return $ (dir </>) <$> fs

-- | Get a list of relative paths to all files (not directories) inside the
-- given directory and all subdirectories.
listAllFilesRelative :: FilePath -> IO [FilePath]
listAllFilesRelative parent = do
  names <- listDirectory parent
  files <- filterM (doesFileExist . (parent </>)) names
  dirs <- filterM (doesDirectoryExist . (parent </>)) names
  (files ++) . concat <$> mapM recur dirs
  where
    recur subd = do
      ps <- listAllFilesRelative (parent </> subd)
      return $ (subd </>) <$> ps
