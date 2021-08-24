module Adatipd.Cardano
  ( -- * Addresses
    Address (..)
  , formatAddress

    -- * Amounts
  , Lovelace (..)
  , formatAda
  ) where

import Data.Scientific (FPFormat (Fixed), formatScientific, scientific)
import Data.Text (Text)

--------------------------------------------------------------------------------
-- Addresses

-- TODO: Address format should be restricted to Bech32 strings.
--       (Probably means making the constructor private.)
newtype Address =
  Address Text

formatAddress :: Address -> Text
formatAddress (Address bech32) = bech32

--------------------------------------------------------------------------------
-- Amounts

newtype Lovelace =
  Lovelace Integer

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
