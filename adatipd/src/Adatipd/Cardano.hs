{-# LANGUAGE OverloadedStrings #-}

module Adatipd.Cardano
  ( -- * Addresses
    Address (..)
  , formatBech32

    -- * Amounts
  , Lovelace (..)
  , formatAda

    -- * Payment URIs
  , paymentUri
  ) where

import Data.Scientific (FPFormat (Fixed), formatScientific, scientific)
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

formatAda :: Lovelace -> String
formatAda (Lovelace lovelace) =
  let
    decimal =
      formatScientific
        Fixed   -- Standard decimal notation.
        Nothing -- Do not restrict number of decimals.
        (scientific lovelace (-6))
  in
    "₳" <> decimal

--------------------------------------------------------------------------------
-- Payment URIs

-- |
-- Construct a CIP 13 [1] URI for Cardano payments.
-- [1]: https://cips.cardano.org/cips/cip13/
paymentUri :: Address -> Lovelace -> TLB.Builder
paymentUri address (Lovelace lovelace) =
  "web+cardano:"
  <> TLB.fromText (formatBech32 address)
  <> "?amount="
  <> formatScientificBuilder Fixed Nothing (scientific lovelace (-6))
