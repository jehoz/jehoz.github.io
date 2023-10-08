module Website.IO where

import Control.Monad (filterM, forM, forM_)
import Data.List (partition)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)
import System.Exit (die)
import System.FilePath (isExtensionOf, takeDirectory, (-<.>), (</>))
import Text.Blaze.Html (toHtml)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Website.Parsers
import Website.Types

readContent :: FilePath -> IO Website
readContent contentDir = do
  relPaths <- getRelativePathsInside contentDir

  let (mdFiles, otherFiles) = partition (".md" `isExtensionOf`) relPaths

  articles <- forM mdFiles $ \rpath -> do
    mdText <- TIO.readFile (contentDir </> rpath)
    case parseArticle mdText of
      Left err -> die ("Error parsing " <> rpath <> "\n" <> T.unpack err)
      Right (Article props nodes) ->
        return $ Article (props {articlePath = rpath}) nodes

  return $ Website contentDir articles otherFiles

buildWebsite :: Website -> FilePath -> IO ()
buildWebsite (Website sourceDir articles staticFiles) outputDir = do
  -- render html pages for articles
  forM_ articles $ \(Article props node) -> do
    let writePath = outputDir </> articlePath props -<.> "html"
        htmlText = renderHtml (toHtml node)
    createDirectoryIfMissing True (takeDirectory writePath)
    TIO.writeFile writePath (TL.toStrict htmlText)

  -- copy static files
  forM_ staticFiles $ \rpath -> do
    let fromPath = sourceDir </> rpath
        toPath = outputDir </> rpath
    createDirectoryIfMissing True (takeDirectory toPath)
    copyFile fromPath toPath

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
