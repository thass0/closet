module Lib
    ( generate
    ) where

import qualified ProjectConfig

generate :: FilePath -> IO ()
generate path = do
    putStrLn $ "Generating site into " <> path <> " ..."
    config <- ProjectConfig.readConfig "examples/simple/_config.yml"
    print config
