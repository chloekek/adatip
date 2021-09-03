-- SPDX-License-Identifier: AGPL-3.0-only

{-# LANGUAGE OverloadedStrings #-}

module Adatipd.Web.AdminStatus
  ( handleAdminStatus
  ) where

import Adatipd.Cardano.Wallet (NetworkInfo (..), SyncProgress (..), SyncStatus (..), withWallet)
import Adatipd.Options (Options (..))
import Adatipd.Web.Layout (renderLayout)
import Text.Blaze (Markup, (!))

import qualified Adatipd.Cardano.Wallet as Wallet
import qualified Adatipd.WaiUtil as Wai
import qualified Data.Text as Text
import qualified Network.HTTP.Types.Status as Status
import qualified Text.Blaze as HB
import qualified Text.Blaze.Html5 as HH
import qualified Text.Blaze.Html5.Attributes as HA

handleAdminStatus :: Options -> Wai.Application
handleAdminStatus options _request writeResponse = do
  networkInfo <- withWallet $ Wallet.getNetworkInfo
  writeResponse $
    Wai.responseHtml Status.ok200 [] $
      renderAdminStatus options networkInfo

renderAdminStatus
  :: Options
  -> NetworkInfo
  -> Markup
renderAdminStatus options networkInfo =
  let
    title = "Instance Status"
    renderChainTip name tip = do
      HH.tr $ do
        HH.td $ HB.text $ name <> " time"
        HH.td $ HB.text $ Text.pack $ show $ Wallet.ctTime tip
      HH.tr $ do
        HH.td $ HB.text $ name <> " epoch"
        HH.td $ HB.text $ Text.pack $ show $ Wallet.ctEpochNumber tip
      HH.tr $ do
        HH.td $ HB.text $ name <> " slot (absolute)"
        HH.td $ HB.text $ Text.pack $ show $ Wallet.ctAbsoluteSlotNumber tip
      HH.tr $ do
        HH.td $ HB.text $ name <> " slot (in epoch)"
        HH.td $ HB.text $ Text.pack $ show $ Wallet.ctSlotInEpoch tip

  in
    renderLayout options title $ do
      HH.header ! HA.class_ "admin-banner" $ do
        HH.h1 $ HB.text "Instance Status"

      HH.section ! HA.class_ "admin-page" $ do
        HH.h1 $ HB.text "Cardano Wallet"
        HH.table $ do
          HH.tr $ do
            HH.td $ HB.text "Wallet sync status"
            HH.td $ HB.text $ case Wallet.niSyncProgress networkInfo of
              SyncProgress SyncReady   -> "ready"
              SyncProgress SyncSyncing -> "syncing"
          case Wallet.niNetworkTip networkInfo of
            Just tip -> renderChainTip "Network" tip
            Nothing  -> do
              HH.tr $ do
                HH.td $ HB.text "Network tip"
                HH.td $ HB.text "unavailable"

          renderChainTip "Node" $ Wallet.niNodeTip networkInfo
