module Adatipd.CardanoSpec
  ( spec
  ) where

import Adatipd.Cardano (Lovelace (..), formatAda)
import Data.Foldable (for_)
import Test.Hspec (Spec, it, shouldBe)

examples :: [(Lovelace, String)]
examples =
  [ (Lovelace 0, "₳0.0")
  , (Lovelace 1, "₳0.000001")
  , (Lovelace 2_500_000, "₳2.5")
  , (Lovelace 2_500_000_000, "₳2500.0") ]

spec :: Spec
spec =

  for_ examples $
    \(lovelace, expected) ->
      it ("formatAda formats (" <> show lovelace <> ") correctly") $
        formatAda lovelace `shouldBe` expected
