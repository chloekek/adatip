-- SPDX-License-Identifier: AGPL-3.0-only

{-# LANGUAGE OverloadedStrings #-}

module Adatipd.CardanoSpec
  ( spec
  ) where

import Adatipd.Cardano.Token (Lovelace (..), formatAdaWithSymbol)
import Adatipd.Cardano.Wallet (NetworkInfo (..), SyncProgress (..), SyncStatus (..), ChainTip (..))
import Data.Aeson (FromJSON)
import Data.Foldable (for_)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Expectations (Expectation, HasCallStack)

import qualified Data.Aeson as Aeson
import qualified Data.Text.Lazy.Builder as TLB (Builder)

-- | Check that an example input in test-unit/data/«path» deserializes to the
-- expected value.
shouldDeserializeTo
  :: HasCallStack
  => FromJSON a
  => Show a
  => Eq a
  => FilePath
  -> a
  -> Expectation
shouldDeserializeTo path expectedOutcome = do
  result <- Aeson.eitherDecodeFileStrict' ("test-unit/data/" <> path)
  result `shouldBe` Right expectedOutcome

spec :: Spec
spec = do

  describe "Token.formatAdaWithSymbol" $
    let
      examples :: [(Lovelace, TLB.Builder)]
      examples =
        [ (Lovelace 0, "₳0.0")
        , (Lovelace 1, "₳0.000001")
        , (Lovelace 2_500_000, "₳2.5")
        , (Lovelace 2_500_000_000, "₳2500.0") ]
    in
      for_ examples $ \(lovelace, expected) ->
        it ("formats (" <> show lovelace <> ") correctly") $
          formatAdaWithSymbol lovelace `shouldBe` expected

  describe "Wallet.NetworkInfo" $ do
    it "deserializes example 1" $
      "v2-network-information-1.json" `shouldDeserializeTo`
      NetworkInfo
        { niNetworkTip = Just ChainTip
          { ctTime = read "2021-09-02 15:27:18Z"
          , ctEpochNumber = 154
          , ctAbsoluteSlotNumber = 36227222
          , ctSlotInEpoch = 68822
          }
        , niNodeTip = ChainTip
          { ctTime = read "2019-07-24 20:20:16Z"
          , ctEpochNumber = 0
          , ctAbsoluteSlotNumber = 0
          , ctSlotInEpoch = 0
          }
        , niSyncProgress = SyncProgress SyncSyncing
        }

    it "deserializes example 2" $
      "v2-network-information-2.json" `shouldDeserializeTo`
      NetworkInfo
        { niNetworkTip = Just ChainTip
          { ctTime = read "2021-09-02 15:30:55Z"
          , ctEpochNumber = 154
          , ctAbsoluteSlotNumber = 36227439
          , ctSlotInEpoch = 69039
          }
        , niNodeTip = ChainTip
          { ctTime = read "2021-09-02 15:29:40Z"
          , ctEpochNumber = 154
          , ctAbsoluteSlotNumber = 36227364
          , ctSlotInEpoch = 68964
          }
        , niSyncProgress = SyncProgress SyncReady
        }

    it "deserializes example 3" $
      "v2-network-information-3.json" `shouldDeserializeTo`
      NetworkInfo
        { niNetworkTip = Nothing
        , niNodeTip = ChainTip
          { ctTime = read "2021-08-28 01:54:42Z"
          , ctEpochNumber = 286
          , ctAbsoluteSlotNumber = 38549391
          , ctSlotInEpoch = 360591
          }
        , niSyncProgress = SyncProgress SyncSyncing
        }
