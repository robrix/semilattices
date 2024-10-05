module Main
( main
) where

import System.Environment
import Test.DocTest
import Test.QuickCheck () -- NB: Workaround for a bug in ghc 9.8 CI
import Test.QuickCheck.Instances ()

main :: IO ()
main = do
  args <- getArgs
  doctest ("-isrc" : "--fast" : if null args then ["src"] else args)
