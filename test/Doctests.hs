module Main where

import Test.DocTest
import System.Environment

main :: IO ()
main = getArgs >>= doctest . (++ ["-isrc", "src"])
