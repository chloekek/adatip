{-# LANGUAGE OverloadedStrings #-}

module Adatipd.Cardano
  ( -- * Addresses
    Address (..)
  , formatBech32

    -- * Amounts
  , Lovelace (..)
  , lovelaceToAda
  , formatAdaDecimal
  , formatAdaWithSymbol

    -- * Payment URIs
  , paymentUri
  ) where

import Data.Scientific (FPFormat (Fixed), Scientific, scientific)
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
-- Convert an amount to its Ada value.
lovelaceToAda :: Lovelace -> Scientific
lovelaceToAda (Lovelace lovelace) =
  scientific lovelace (-6)

-- |
-- Format an amount as its Ada value.
formatAdaDecimal :: Lovelace -> TLB.Builder
formatAdaDecimal lovelace =
  formatScientificBuilder
    Fixed   -- Standard decimal notation.
    Nothing -- Do not restrict number of decimals.
    (lovelaceToAda lovelace)

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
