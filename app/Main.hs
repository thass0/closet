module Main (main) where

import  System.Environment

import Lib

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> generate "_site"
        [into] -> generate into
        moreArgs -> do
            me <- getProgName
            putStrLn $ me <> ": too many arguments " <>
                show moreArgs
