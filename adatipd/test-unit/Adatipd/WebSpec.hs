-- SPDX-License-Identifier: AGPL-3.0-only

{-# LANGUAGE OverloadedStrings #-}

module Adatipd.WebSpec
  ( spec
  ) where

import Adatipd.Sql.Test (withSqlConnection)
import Test.Hspec (Spec, it, shouldBe)

import qualified Adatipd.Sql as Sql
import qualified Hasql.Decoders as SqlDec
import qualified Hasql.Encoders as SqlEnc

spec :: Spec
spec = do

  it "handle redirects to the latest nickname" $
    withSqlConnection $
      \sqlConn -> do
        -- TODO: Replace this with a test that checks nickname redirects.
        result <-
          Sql.runSession sqlConn $
            Sql.statement () $
              Sql.Statement
                "SELECT 42"
                SqlEnc.noParams
                (SqlDec.singleRow (SqlDec.column (SqlDec.nonNullable SqlDec.int8)))
                False
        result `shouldBe` 42
