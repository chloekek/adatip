-- SPDX-License-Identifier: AGPL-3.0-only

{-# LANGUAGE OverloadedStrings #-}

module Adatipd.Web.CreatorTipSuggestions
  ( handleCreatorTipSuggestions
  ) where

import Adatipd.Cardano.Address (Address (..), formatBech32)
import Adatipd.Cardano.Token (Lovelace (..), formatAdaWithSymbol)
import Adatipd.Creator (CreatorId, CreatorInfo (..), fetchCreatorInfo)
import Adatipd.Web.Context (Context (..), Options (..))
import Adatipd.Web.CreatorLayout (CreatorTab (..), renderCreatorLayout)
import Codec.QRCode (QRImage)
import Data.Foldable (for_)
import Data.Maybe (isNothing)
import Data.Vector (Vector)
import Network.HTTP.Types.Status (status200)
import Text.Blaze (Markup, (!))

import qualified Adatipd.Cardano.Util as Cardano (paymentUri)
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
    , ctsTipSuggestions :: Vector TipSuggestion
    , ctsCustomAmountQrImage :: Maybe QRImage }

data TipSuggestion =
  TipSuggestion
    { tsAmount :: Lovelace
    , tsQrImage :: Maybe QRImage
      -- ^ QR code creation may fail, so this is 'Maybe'.
    }

mkTipSuggestion :: Lovelace -> Address -> TipSuggestion
mkTipSuggestion tsAmount address =
  TipSuggestion
    { tsAmount
    , tsQrImage = mkQrImage (Just tsAmount) address }

mkQrImage :: Maybe Lovelace -> Address -> Maybe QRImage
mkQrImage amount address =
  let
    -- High error correction leads to an enormous QR code.
    -- Medium should be plenty; people will scan this with their phone from a
    -- monitor, not from a badly printed piece of paper with coffee stains on
    -- it. And also, a bech32 address has a checksum in it, so even if the QR
    -- code fails to transfer properly, it will not suddenly transfer to the
    -- wrong address.
    qrOptions = Qr.defaultQRCodeOptions Qr.M
    qrText = TLB.toLazyText (Cardano.paymentUri address amount)
  in
    Qr.encodeText qrOptions Qr.Iso8859_1 qrText

--------------------------------------------------------------------------------
-- Request handling

handleCreatorTipSuggestions
  :: Context -> Sql.Connection -> CreatorId -> Wai.Application
handleCreatorTipSuggestions context sqlConn creatorId _request writeResponse = do
  creatorTipSuggestions <- fetchCreatorTipSuggestions sqlConn creatorId
  writeResponse $
    Wai.responseHtml status200 [] $
      renderCreatorTipSuggestions context creatorTipSuggestions

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
          [ mkTipSuggestion (Lovelace 1000000) ctsTipAddress
          , mkTipSuggestion (Lovelace 2000000) ctsTipAddress
          , mkTipSuggestion (Lovelace 3000000) ctsTipAddress
          , mkTipSuggestion (Lovelace 4000000) ctsTipAddress ]

  let ctsCustomAmountQrImage =
        mkQrImage Nothing ctsTipAddress

  pure CreatorTipSuggestions {..}

renderCreatorTipSuggestions :: Context -> CreatorTipSuggestions -> Markup
renderCreatorTipSuggestions context CreatorTipSuggestions {..} =
  renderCreatorLayout context CreatorTipsTab ctsCreatorInfo $
    HH.section ! HA.class_ "creator-tip-suggestions" $ do
      renderTutorial ctsCreatorInfo
      renderTipSuggestions ctsTipSuggestions ctsCustomAmountQrImage
      renderTipAddress ctsCreatorInfo ctsTipAddress
      renderFinePrint ctsCreatorInfo context

renderTutorial :: CreatorInfo -> Markup
renderTutorial CreatorInfo {..} =
  HH.p ! HA.class_ "-tutorial" $ do

    "Feeling generous? Send "
    HH.strong $ HB.text ciName
    " a tip! 😍"
    HH.br

    "Select an amount, then scan the QR code with your wallet."

renderTipSuggestions :: Vector TipSuggestion -> Maybe QRImage -> Markup
renderTipSuggestions tipSuggestions customAmountQrImage =
  HH.section ! HA.class_ "-tip-suggestions" $ do

    for_ ([1 ..] `zip` Vector.toList tipSuggestions) $
      \(i, TipSuggestion {..}) ->
        renderTipSuggestion i (Just tsAmount) tsQrImage

    renderTipSuggestion 0 Nothing customAmountQrImage

renderTipSuggestion :: Int -> Maybe Lovelace -> Maybe QRImage -> Markup
renderTipSuggestion i amount qrImage = do

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
    ! if isNothing amount then HA.checked "checked" else mempty

  HH.label ! HA.class_ "-label" ! HA.for idAttr $
    case amount of
      Just amount' -> HB.textBuilder (formatAdaWithSymbol amount')
      Nothing -> "Custom amount"

  case qrImage of
    Nothing ->
      HB.stringComment
        "Unfortunately no QR code could \
        \be generated for this address."
    Just qrImage' -> do
      let qrImagePng = Qr.toPngDataUrlT 0 8 qrImage'
      HH.img
        ! HA.class_ "-qr-code"
        ! HA.src (HB.lazyTextValue qrImagePng)

renderTipAddress :: CreatorInfo -> Address -> Markup
renderTipAddress CreatorInfo {..} tipAddress =
  HH.p ! HA.class_ "-tip-address" $ do
    "You can also send directly to the Cardano address of "
    HB.text ciName *> ": "
    HH.code $ HB.text (formatBech32 tipAddress)

renderFinePrint :: CreatorInfo -> Context -> Markup
renderFinePrint CreatorInfo {..} Context {..} = do
  let Options {..} = cOptions

  HH.p ! HA.class_ "-fine-print" $ do
    "When you send a tip, you send Ada directly to "
    HB.text ciName *> ". "

    HB.text ciName *> " will receive the full tipped amount: "
    HH.text oInstanceTitle *> " does not process the tip and "
    "does not charge you for sending tips. "

    "Tips are voluntary and do not grant access to exclusive content "
    "on " *> HB.text oInstanceTitle *> ". "

    "The Cardano network may additionally charge a transaction fee. "
    "Your wallet will show you the fee before you sign the transaction. "

    "Only send Cardano assets, such as Ada, to the address. "
    "Do not send assets of other cryptocurrency platforms "
    "such as Bitcoin or Ethereum to the address; "
    "they will not arrive and they will be lost unrecoverably."
