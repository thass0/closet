module Resources (copyResources, listFilesRecurse) where

import Control.Monad
import System.Directory
import System.FilePath

copyResources
  :: FilePath
  -- ^ Source directory
  -> FilePath
  -- ^ Destination directory
  -> [FilePath]
  -- ^ Top-level paths to ignore
  -> IO ()
copyResources src dest exclude = do
  entries <- listDirectory src
  let includedEntries = filter (`notElem` exclude) entries
  toCopy <- concat <$> forM includedEntries getFilesToCopy
  prepareDest
  forM_ toCopy (copyFileRelative src dest)
  where
    getFilesToCopy :: FilePath -> IO [FilePath]
    getFilesToCopy entry = do
      let path = src </> entry
      isDir <- doesDirectoryExist path
      files <-
        if isDir
          then listFilesRecurse path
          else pure [path]
      pure $ makeRelative src <$> files

    prepareDest :: IO ()
    prepareDest = do
      exists <- doesPathExist dest
      if exists
        then error $ "Generate destination " <> dest <> " already exists"
        else createDirectory dest

copyFileRelative
  :: FilePath
  -- ^ Source directory
  -> FilePath
  -- ^ Destination directory
  -> FilePath
  -- ^ File path relative to the source and destination
  -> IO ()
copyFileRelative src dest rel = do
  let from = src </> rel
      to = dest </> rel
  createDirectoryIfMissing True $ takeDirectory to
  copyFile from to

-- | Recursively list all files starting at the given directory.
--   Doesn't follow symbolic links and doesn't include @.@ and @..@.
listFilesRecurse :: FilePath -> IO [FilePath]
listFilesRecurse startDir = do
  dirEntries <- listDirectory startDir
  subDirEntries <-
    forM
      dirEntries
      ( \entry -> do
          let path = startDir </> entry
          isDir <- doesDirectoryExist path
          if isDir
            then listFilesRecurse path
            else pure [path]
      )
  pure $ concat subDirEntries
