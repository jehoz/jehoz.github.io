module FileUtils where

import Control.Monad (filterM)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>))


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
