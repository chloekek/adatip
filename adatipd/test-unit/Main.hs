module Main
  ( main
  ) where

import Test.Hspec (parallel)
import Test.Hspec.Runner (defaultConfig, hspecWith)

import qualified Spec (spec)

main :: IO ()
main =
  hspecWith defaultConfig $
    -- We can run all tests in parallel.
    parallel Spec.spec
