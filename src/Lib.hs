module Lib (
  generate,
) where

import qualified CanonPath
import ProjectConfig (configFileName, initConfig)
import Resources (listFilesRecurse)

{-
import qualified Data.Yaml as Y
import Data.Text
import Data.List.NonEmpty
import qualified Data.HashMap.Lazy as M
import Control.Monad
import Doc

-- | In-memory representation of the generation.
newtype Gen = Gen (M.HashMap GenItemId GenItem)
newtype GenItemId = GenItemId Int deriving (Show, Eq)

-- | An item of data to generate from.
data GenItem
  = GenItem
    { rawPath :: CanonPath.CanonPath
    , rawData :: Text
    }
  | GenData
    { genData :: Y.Object
    }
  | GenMarkupFile
    { markupPath :: CanonPath.CanonPath
    , markupState :: GenMarkupState
    }

data GenMarkupState
  -- | The document tree is ready to be used.
  = MarkupFinished { markupTree :: DocTree }
  -- | The document tree cannot yet be used, because the document
  --   depends on the list of items to be generated first.
  | MarkupWaiting { markupDeps :: NonEmpty GenItemId }
  -- | The document tree has not yet been generated.
  | MarkupReady { markupData :: Text }

  -- I need to build a partial markup tree that contains
  -- fillers where the dependencies will be inserted.
-}

-- | Generate the site in the directory `src` into `dest`.
generate :: FilePath -> FilePath -> IO ()
generate src dest = do
  putStrLn $ "Generating " <> src <> " into " <> dest <> " ..."
  config <- initConfig src configFileName
  print config
  files <- listFilesRecurse src >>= mapM CanonPath.fromPath
  print files
