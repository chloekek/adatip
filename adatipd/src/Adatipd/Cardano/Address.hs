-- SPDX-License-Identifier: AGPL-3.0-only

{-# LANGUAGE OverloadedStrings #-}

module Adatipd.Cardano.Address
  ( Address (..)
  , formatBech32
  ) where

import Data.Text (Text)

-- TODO: Address format should be restricted to Bech32 strings.
--       (Probably means making the constructor private.)
newtype Address =
  Address Text

formatBech32 :: Address -> Text
formatBech32 (Address bech32) = bech32
