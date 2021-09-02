-- SPDX-License-Identifier: AGPL-3.0-only

{-# LANGUAGE OverloadedStrings #-}

module Adatipd.Cardano.Util
  ( paymentUri
  ) where

import Adatipd.Cardano.Address (Address, formatBech32)
import Adatipd.Cardano.Token (Lovelace, formatAdaDecimal)

import qualified Data.Text.Lazy.Builder as TLB (Builder, fromText)

-- |
-- Construct a CIP 13 [1] URI for Cardano payments.
-- [1]: https://cips.cardano.org/cips/cip13/
paymentUri :: Address -> Maybe Lovelace -> TLB.Builder
paymentUri address amount =
  "web+cardano:" <> TLB.fromText (formatBech32 address)
  <> foldMap (\amnt -> "?amount=" <> formatAdaDecimal amnt) amount
