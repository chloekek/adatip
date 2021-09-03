-- SPDX-License-Identifier: AGPL-3.0-only

{-# LANGUAGE OverloadedStrings #-}

module Adatipd.Cardano.Token
  ( Lovelace (..)
  , formatAdaDecimal
  , formatAdaWithSymbol
  ) where

import Data.Scientific (FPFormat (Fixed), scientific)
import Data.Text.Lazy.Builder.Scientific (formatScientificBuilder)

import qualified Data.Text.Lazy.Builder as TLB (Builder)

newtype Lovelace =
  Lovelace Integer
  deriving stock (Show)

-- |
-- Format an amount as its Ada value.
formatAdaDecimal :: Lovelace -> TLB.Builder
formatAdaDecimal (Lovelace lovelace) =
  let
    -- Ada = 1_000_000 Lovelace.
    ada = scientific lovelace (-6)
  in
    formatScientificBuilder
      Fixed   -- Standard decimal notation.
      Nothing -- Do not restrict number of decimals.
      ada

-- |
-- Format an amount as its Ada value
-- prefixed by the ‘₳’ currency symbol.
formatAdaWithSymbol :: Lovelace -> TLB.Builder
formatAdaWithSymbol lovelace = "₳" <> formatAdaDecimal lovelace
