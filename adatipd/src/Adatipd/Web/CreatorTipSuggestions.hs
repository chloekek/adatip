-- SPDX-License-Identifier: AGPL-3.0-only

{-# LANGUAGE OverloadedStrings #-}

module Adatipd.Web.CreatorTipSuggestions
  ( handleCreatorTipSuggestions
  ) where

import Adatipd.Cardano (Address (..), Lovelace (..), formatAdaWithSymbol, formatBech32)
import Adatipd.Creator (CreatorId, CreatorInfo (..), fetchCreatorInfo)
import Adatipd.Options (Options (..))
import Adatipd.Web.CreatorLayout (CreatorTab (..), renderCreatorLayout)
import Codec.QRCode (QRImage)
import Data.Foldable (traverse_)
import Data.Maybe (isNothing)
import Data.Vector (Vector)
import Network.HTTP.Types.Status (status200)
import Text.Blaze (Markup, (!))

import qualified Adatipd.Cardano as Cardano (paymentUri)
import qualified Adatipd.Sql as Sql
import qualified Adatipd.WaiUtil as Wai (Application, responseHtml)
import qualified Codec.QRCode as Qr
import qualified Codec.QRCode.JuicyPixels as Qr
import qualified Data.Text.Lazy.Builder as TLB (toLazyText)
import qualified Data.Vector as Vector (fromList, toList)
import qualified Text.Blaze as HB
import qualified Text.Blaze.Html5 as HH
import qualified Text.Blaze.Html5.Attributes as HA
import qualified Text.Blaze.Internal as HB (textBuilder)

--------------------------------------------------------------------------------
-- Retrieving tip suggestions

data CreatorTipSuggestions =
  CreatorTipSuggestions
    { ctsCreatorInfo :: CreatorInfo
    , ctsTipAddress :: Address
    , ctsTipSuggestions :: Vector TipSuggestion }

data TipSuggestion =
  TipSuggestion
    { tsAmount :: Maybe Lovelace
      -- ^ 'Nothing' for the “custom amount” option.
    , tsQrImage :: Maybe QRImage
      -- ^ QR code creation may fail, so this is 'Maybe'.
    }

-- |
-- Create a tip suggestion from its parameters.
-- QR image will be generated automatically
-- and does not need to be given to this function.
mkTipSuggestion :: Maybe Lovelace -> Address -> TipSuggestion
mkTipSuggestion tsAmount address =
  let
    -- High error correction leads to an enormous QR code.
    -- Medium should be plenty; people will scan this with their phone from a
    -- monitor, not from a badly printed piece of paper with coffee stains on
    -- it. And also, a bech32 address has a checksum in it, so even if the QR
    -- code fails to transfer properly, it will not suddenly transfer to the
    -- wrong address.
    qrOptions = Qr.defaultQRCodeOptions Qr.M
    qrText = TLB.toLazyText (Cardano.paymentUri address tsAmount)
    tsQrImage = Qr.encodeText qrOptions Qr.Iso8859_1 qrText
  in
    TipSuggestion {..}

--------------------------------------------------------------------------------
-- Request handling

handleCreatorTipSuggestions
  :: Options -> Sql.Connection -> CreatorId -> Wai.Application
handleCreatorTipSuggestions options sqlConn creatorId _request writeResponse = do
  creatorTipSuggestions <- fetchCreatorTipSuggestions sqlConn creatorId
  writeResponse $
    Wai.responseHtml status200 [] $
      renderCreatorTipSuggestions options creatorTipSuggestions

fetchCreatorTipSuggestions
  :: Sql.Connection -> CreatorId -> IO CreatorTipSuggestions
fetchCreatorTipSuggestions sqlConn creatorId = do

  ctsCreatorInfo <- fetchCreatorInfo sqlConn creatorId

  let ctsTipAddress =
        Address
          "addr1qy7h4lxjsn7cz95gen79upe3lls6wlqn35m\
          \yruevx37utqal5fedzlcnfduhaqlqnyamuyt8apy\
          \6pfj6qu4fj8dmr4tsa3g6qz"

  let ctsTipSuggestions =
        Vector.fromList
          [ mkTipSuggestion (Just (Lovelace 1000000)) ctsTipAddress
          , mkTipSuggestion (Just (Lovelace 2000000)) ctsTipAddress
          , mkTipSuggestion (Just (Lovelace 3000000)) ctsTipAddress
          , mkTipSuggestion (Just (Lovelace 4000000)) ctsTipAddress
          , mkTipSuggestion Nothing ctsTipAddress ]

  pure CreatorTipSuggestions {..}

renderCreatorTipSuggestions :: Options -> CreatorTipSuggestions -> Markup
renderCreatorTipSuggestions options CreatorTipSuggestions {..} =
  renderCreatorLayout options CreatorTipsTab ctsCreatorInfo $
    HH.section ! HA.class_ "creator-tip-suggestions" $ do
      renderTutorial ctsCreatorInfo
      renderTipSuggestions ctsTipSuggestions
      renderTipAddress ctsCreatorInfo ctsTipAddress
      renderFinePrint options

renderTutorial :: CreatorInfo -> Markup
renderTutorial CreatorInfo {..} =
  HH.p ! HA.class_ "-tutorial" $ do

    "Feeling generous? Send "
    HH.strong $ HB.text ciName
    " a tip! 😍"
    HH.br

    "Select an amount, then scan the QR code with your wallet."

renderTipSuggestions :: Vector TipSuggestion -> Markup
renderTipSuggestions tipSuggestions =
  HH.section ! HA.class_ "-tip-suggestions" $
    traverse_
      (uncurry renderTipSuggestion)
      ([0 ..] `zip` Vector.toList tipSuggestions)

renderTipSuggestion :: Int -> TipSuggestion -> Markup
renderTipSuggestion i TipSuggestion {..} = do

  -- For each tip suggestion we render a radio button, a label, and a QR code.
  -- The label, when clicked, will mark the radio button as checked.
  -- The QR code is hidden until the associated radio is checked.
  -- This is achieved using CSS ‘:checked’ and sibling selectors.

  let idAttr = HB.toValue ("tip-suggestion-" <> show i)

  HH.input
    ! HA.id idAttr
    ! HA.class_ "-radio"
    ! HA.type_ "radio"
    ! HA.name "tip-suggestion-radio"
    ! if isNothing tsAmount then HA.checked "checked" else mempty

  HH.label ! HA.class_ "-label" ! HA.for idAttr $
    case tsAmount of
      Just amount -> HB.textBuilder (formatAdaWithSymbol amount)
      Nothing -> "Custom amount"

  case tsQrImage of
    Nothing ->
      HB.stringComment
        "Unfortunately no QR code could \
        \be generated for this address."
    Just qrImage -> do
      let qrImagePng = Qr.toPngDataUrlT 4 8 qrImage
      HH.img
        ! HA.class_ "-qr-code"
        ! HA.src (HB.lazyTextValue qrImagePng)

renderTipAddress :: CreatorInfo -> Address -> Markup
renderTipAddress CreatorInfo {..} tipAddress =
  HH.p ! HA.class_ "-tip-address" $ do
    "You can also send directly to the Cardano address of "
    HB.text ciName *> ": "
    HH.code $ HB.text (formatBech32 tipAddress)

renderFinePrint :: Options -> Markup
renderFinePrint Options {..} =
  HH.p ! HA.class_ "-fine-print" $ do
    "Tips do not grant access to exclusive content." *> HH.br
    "Do not send coins other than Ada to the address." *> HH.br
    HH.text oInstanceTitle *> " will not charge you for sending tips."
