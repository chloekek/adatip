-- SPDX-License-Identifier: AGPL-3.0-only

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- | This module exposes types and functions to interact with cardano-wallet's
-- REST API, which allows things like listing UTXOs for an address, and
-- constructing new transactions.
--
-- Cardano-wallet internally uses Servant for this and it has a client, but it
-- is difficult to use outside of cardano-wallet itself, so instead we write our
-- own types and serializers and we interact with the API directly.
module Adatipd.Cardano.Wallet
  ( ChainTip (..)
  , NetworkInfo (..)
  , SyncProgress (..)
  , SyncStatus (..)
  , WalletIO
  , withWallet
  , getNetworkInfo
  ) where

import Control.Monad (when)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans (lift)
import Data.Aeson (FromJSON, (.:))
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Network.HTTP.Types.Method (Method, methodGet)

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Types.Status as Status

data ChainTip = ChainTip
  { ctTime :: UTCTime
  , ctEpochNumber :: Int
  , ctAbsoluteSlotNumber :: Int
  , ctSlotInEpoch :: Int
  } deriving (Eq, Show)

instance FromJSON ChainTip where
  parseJSON = Aeson.withObject "ChainTip" $ \v -> ChainTip
    <$> v .: "time"
    <*> v .: "epoch_number"
    <*> v .: "absolute_slot_number"
    <*> v .: "slot_number"

data SyncStatus
  = SyncSyncing
  | SyncReady
  deriving (Eq, Show)

instance FromJSON SyncStatus where
  parseJSON = Aeson.withText "SyncStatus" $ \v -> case v of
    "ready"   -> pure SyncReady
    "syncing" -> pure SyncSyncing
    other     -> fail $ "Unrecognized sync status: " <> Text.unpack other

newtype SyncProgress =
  SyncProgress SyncStatus
  deriving (Eq, Show)

instance FromJSON SyncProgress where
  parseJSON = Aeson.withObject "SyncProgress" $ \v -> SyncProgress
    <$> v .: "status"

data NetworkInfo = NetworkInfo
  { niNetworkTip :: ChainTip
  , niNodeTip :: ChainTip
  , niSyncProgress :: SyncProgress
  } deriving (Eq, Show)

instance FromJSON NetworkInfo where
  parseJSON = Aeson.withObject "NetworkInfo" $ \v -> NetworkInfo
    <$> v .: "network_tip"
    <*> v .: "node_tip"
    <*> v .: "sync_progress"

makeRequest
  :: FromJSON a
  => Http.Manager
  -> Method
  -> Text
  -> IO a
makeRequest manager method url = do
  request <- Http.parseRequest $ Text.unpack url
  response <- Http.httpLbs (request { Http.method = method }) manager

  when (Http.responseStatus response /= Status.ok200) $
    fail $ "Got unexpected response for " <> (Text.unpack url) <> ": " <> (show response)

  case Aeson.eitherDecode' (Http.responseBody response) of
    Left err -> fail $ "Failed to decode json for " <> (Text.unpack url) <> ": " <> err
    Right result -> pure result

newtype WalletIO a = WalletIO (ReaderT Http.Manager IO a)

withWallet :: WalletIO a -> IO a
withWallet (WalletIO f) = do
  manager <- Http.newManager Http.defaultManagerSettings
  runReaderT f manager

makeRequest' :: FromJSON a => Method -> Text -> WalletIO a
makeRequest' method url = WalletIO $ do
  manager <- ask
  -- TODO: Make the wallet host and address configurable.
  lift $ makeRequest manager method ("http://127.0.0.1:8083" <> url)

-- | Return information about the network the wallet is connected to.
getNetworkInfo :: WalletIO NetworkInfo
getNetworkInfo = makeRequest' methodGet "/v2/network/information"
