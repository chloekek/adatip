module Main
  ( main
  ) where

import Adatipd.Options (optionsParserInfo)
import Adatipd.Web (handle)
import Options.Applicative (execParser)

import qualified Network.Wai.Handler.Warp as Warp (run)

main :: IO ()
main = do
  options <- execParser optionsParserInfo
  print options
  Warp.run 8081 (handle options)
