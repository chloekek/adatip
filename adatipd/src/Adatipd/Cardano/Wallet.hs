-- SPDX-License-Identifier: AGPL-3.0-only

{-# LANGUAGE OverloadedStrings #-}

module Adatipd.Cardano.Wallet
  ( ChainTip (..)
  , SyncStatus (..)
  , NetworkInfo (..)
  ) where

import Data.Aeson (FromJSON, (.:))
import Data.Time.Clock (UTCTime)
import Data.Text (Text)

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text

data ChainTip = ChainTip
  { ctTime :: UTCTime
  , ctEpochNumber :: Int
  , ctAbsoluteSlotNumber :: Int
  , ctSlotInEpoch :: Int
  }

instance FromJSON ChainTip where
  parseJSON = Aeson.withObject "ChainTip" $ \v -> ChainTip
    <$> v .: "time"
    <*> v .: "epoch_number"
    <*> v .: "absolute_slot_number"
    <*> v .: "slot_number"

data SyncStatus
  = SyncSyncing
  | SyncReady

instance FromJSON SyncStatus where
  parseJSON = Aeson.withText "SyncStatus" $ \v -> case v of
    "ready"   -> pure SyncReady
    "syncing" -> pure SyncSyncing
    other     -> fail $ "Unrecognized sync status: " <> Text.unpack other

newtype SyncProgress = SyncProgress SyncStatus

instance FromJSON SyncProgress where
  parseJSON = Aeson.withObject "SyncProgress" $ \v -> SyncProgress
    <$> v .: "status"

data NetworkInfo = NetworkInfo
  { niNetworkTip :: ChainTip
  , niNodeTip :: ChainTip
  , niSyncProgress :: SyncProgress
  }

instance FromJSON NetworkInfo where
  parseJSON = Aeson.withObject "NetworkInfo" $ \v -> NetworkInfo
    <$> v .: "network_tip"
    <*> v .: "node_tip"
    <*> v .: "sync_progress"
