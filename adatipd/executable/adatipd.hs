-- SPDX-License-Identifier: AGPL-3.0-only

{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import Adatipd.Options (optionsParserInfo)
import Adatipd.Web (handle)
import Options.Applicative (execParser)

import qualified Adatipd.Sql as Sql
import qualified Network.Wai.Handler.Warp as Warp (run)

main :: IO ()
main = do
  options <- execParser optionsParserInfo
  print options
  Warp.run 8081 $
    \request writeResponse ->
      Sql.withConnection "" $
        \sqlConn ->
          handle options sqlConn request writeResponse
