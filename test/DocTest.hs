module Main (main) where

import System.FilePath.Glob
import Test.DocTest

main :: IO ()
main = do
    let options =
            [ "-XOverloadedStrings"
            ]

    paths <- globDir1 (compile "**/*.hs") "src"
    doctest $ options ++ paths
