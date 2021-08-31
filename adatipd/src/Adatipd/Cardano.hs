-- SPDX-License-Identifier: AGPL-3.0-only

{-# LANGUAGE OverloadedStrings #-}

module Adatipd.Cardano
  ( -- * Addresses
    Address (..)
  , formatBech32

    -- * Amounts
  , Lovelace (..)
  , formatAdaDecimal
  , formatAdaWithSymbol

    -- * Payment URIs
  , paymentUri
  ) where

import Data.Scientific (FPFormat (Fixed), scientific)
import Data.Text.Lazy.Builder.Scientific (formatScientificBuilder)

import qualified Data.Text as T (Text)
import qualified Data.Text.Lazy.Builder as TLB (Builder, fromText)

--------------------------------------------------------------------------------
-- Addresses

-- TODO: Address format should be restricted to Bech32 strings.
--       (Probably means making the constructor private.)
newtype Address =
  Address T.Text

formatBech32 :: Address -> T.Text
formatBech32 (Address bech32) = bech32

--------------------------------------------------------------------------------
-- Amounts

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

--------------------------------------------------------------------------------
-- Payment URIs

-- |
-- Construct a CIP 13 [1] URI for Cardano payments.
-- [1]: https://cips.cardano.org/cips/cip13/
paymentUri :: Address -> Maybe Lovelace -> TLB.Builder
paymentUri address amount =
  "web+cardano:" <> TLB.fromText (formatBech32 address)
  <> foldMap (\amnt -> "?amount=" <> formatAdaDecimal amnt) amount
