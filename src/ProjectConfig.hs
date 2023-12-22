module ProjectConfig
    ( ProjectConfig (..)
    , initConfig
    , configFileName
    ) where

import Data.Text
import qualified Data.List as L
import qualified Data.Yaml as Y
import System.Directory (listDirectory)
import System.FilePath ((</>))
import Data.Yaml (FromJSON (..), (.:?), (.!=))

-- NOTE: Email addresses and URLs don't need to be *valid*.
-- You can use whatever you want (e.g. strings such as
-- you (at) closet (dot) blog instead of you@closet.blog).
data ProjectConfig = ProjectConfig
    { title :: Maybe Text
    , email :: Maybe Text
    , author :: Maybe Text
    , description :: Maybe Text
    , baseurl :: Maybe Text
    , twitterUsername :: Maybe Text
    , githubUsername :: Maybe Text
    , plugins :: Maybe [Text]
    , exclude :: Maybe [FilePath]
    }
    deriving (Show, Eq)

instance FromJSON ProjectConfig where
    parseJSON (Y.Object y) =
        ProjectConfig <$>
        y .:? "title" .!= Nothing <*>
        y .:? "email" .!= Nothing <*>
        y .:? "author" .!= Nothing <*>
        y .:? "description" .!= Nothing <*>
        y .:? "baseurl" .!= Nothing <*>
        y .:? "twitter-username" .!= Nothing <*>
        y .:? "github-username" .!= Nothing <*>
        y .:? "plugins" .!= Nothing <*>
        y .:? "exclude" .!= Nothing
    parseJSON _ = fail "Expected YAML object for project configuration"

-- | Default configuration file name.
configFileName :: FilePath
configFileName = "_config.yml"

-- | Initialize the project configuration.
initConfig :: FilePath -> FilePath -> IO ProjectConfig
initConfig src filename = do
  let filepath = src </> filename
  rawEither <- Y.decodeFileEither filepath
  case rawEither of
    Left err -> error $ "Failed to read configuration file "
      <> filepath <> ", " <> Y.prettyPrintParseException err
    Right raw -> do
      de <- defaultExclude
      pure $ raw { exclude = (++ de) <$> raw.exclude }
  where
    defaultExclude :: IO [FilePath]
    defaultExclude = do
      dirConts <- listDirectory src
      pure $ L.filter (L.isPrefixOf "_") dirConts
          
