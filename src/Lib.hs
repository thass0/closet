module Lib
    ( generate
    ) where

import ProjectConfig (configFileName, initConfig)

-- | Generate the site in the directory `src` into `dest`.
generate :: FilePath -> FilePath -> IO ()
generate src dest = do
    putStrLn $ "Generating "<> src <> " into " <> dest <> " ..."
    config <- initConfig src configFileName
    print config

