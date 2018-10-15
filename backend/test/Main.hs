module Main where

import qualified Spec
import Test.Hspec.Formatters
import Test.Hspec.Runner

main :: IO ()
main = hspecWith defaultConfig { configFormatter = Just progress } Spec.spec
