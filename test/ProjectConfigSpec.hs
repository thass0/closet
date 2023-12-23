module ProjectConfigSpec (spec) where

import Test.Hspec

import ProjectConfig

spec :: Spec
spec = do
  describe "Initialize project configuration" $ do
    it "simple config" $ do
      config <- initConfig "examples/simple" "_config.yml"
      config `shouldBe` ProjectConfig
        { title = Just "Closet Blog Title"
        , email = Just "you@closet.blog"
        , author = Just "You, hopefully"
        , description = Just "Default Closet Blog"
        , baseurl = Just "https://closet.blog/"
        , twitterUsername = Just "none"
        , githubUsername = Just "none"
        , plugins = Just ["Blah"]
        , exclude = Just ["Nothing", "_config.yml", "_posts"]
        }
