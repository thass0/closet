module ProjectConfig
    ( ProjectConfig (..)
    , readConfig
    ) where

import Data.Text
import qualified Data.Yaml as Y
import Data.Yaml (FromJSON (..), (.:?), (.!=))
import Control.Applicative

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
    deriving (Show)

readConfig :: FilePath -> IO ProjectConfig
readConfig = Y.decodeFileThrow

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