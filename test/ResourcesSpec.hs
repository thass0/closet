module ResourcesSpec (spec) where

import Test.Hspec
import System.Directory
import System.FilePath
import Control.Monad

import Resources

spec :: Spec
spec = do
  describe "Copy resources files" $ do
    it "simple example" $ do
      tmp <- getTemporaryDirectory
      let testDir = tmp </> "simple-example"
      cleanupDir testDir
      copyResources "examples/simple" testDir ["_config.yml"]
      topLevelConts <- listDirectory testDir
      topLevelConts `shouldMatchList` ["index.md", "_posts", "public"]
      publicConts <- listDirectory $ testDir </> "public/"
      -- `public/` shouldn't contain the `empty/` dir.
      publicConts `shouldMatchList` ["style.css", "data"]
      dataConts <- listDirectory $ testDir </> "public/data/"
      dataConts `shouldMatchList` ["hello.sh"]

cleanupDir :: FilePath -> IO ()
cleanupDir path = do
  exists <- doesDirectoryExist path
  when exists $ removeDirectoryRecursive path
