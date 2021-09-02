-- SPDX-License-Identifier: AGPL-3.0-only

{-# LANGUAGE OverloadedStrings #-}

module Adatipd.CardanoSpec
  ( spec
  ) where

import Adatipd.Cardano.Token (Lovelace (..), formatAdaWithSymbol)
import Data.Foldable (for_)
import Test.Hspec (Spec, it, shouldBe)

import qualified Data.Text.Lazy.Builder as TLB (Builder)

examples :: [(Lovelace, TLB.Builder)]
examples =
  [ (Lovelace 0, "₳0.0")
  , (Lovelace 1, "₳0.000001")
  , (Lovelace 2_500_000, "₳2.5")
  , (Lovelace 2_500_000_000, "₳2500.0") ]

spec :: Spec
spec =

  for_ examples $
    \(lovelace, expected) ->
      it ("formatAdaWithSymbol formats (" <> show lovelace <> ") correctly") $
        formatAdaWithSymbol lovelace `shouldBe` expected
