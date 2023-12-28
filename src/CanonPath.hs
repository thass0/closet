module CanonPath
  ( CanonPath
  , fromPath
  , toPath
  ) where

import System.Directory

-- | An absolute and normalized file path.
newtype CanonPath = CanonPath FilePath deriving (Show)

fromPath :: FilePath -> IO CanonPath
fromPath p = CanonPath <$> canonicalizePath p

toPath :: CanonPath -> FilePath
toPath (CanonPath c) = c
