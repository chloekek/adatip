module Main
  ( main
  ) where

import Adatipd.Options (optionsParserInfo)
import Options.Applicative (execParser)

main :: IO ()
main = execParser optionsParserInfo >>= print
