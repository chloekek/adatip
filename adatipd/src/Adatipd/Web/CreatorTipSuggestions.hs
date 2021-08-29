{-# LANGUAGE OverloadedStrings #-}

module Adatipd.Web.CreatorTipSuggestions
  ( handleCreatorTipSuggestions
  ) where

import Adatipd.Web.CreatorLayout

import Adatipd.Cardano (Address (..), Lovelace (..), formatAdaWithSymbol, formatBech32)
import Adatipd.Nickname (Nickname)
import Adatipd.Options (Options (..))
import Adatipd.Web.NotFound (handleNotFound)
import Codec.QRCode (QRImage)
import Data.Foldable (for_, traverse_)
import Data.Text (Text)
import Data.Vector (Vector)
import Network.HTTP.Types.Status (status200)
import Text.Blaze (Markup, (!))

import qualified Adatipd.Cardano as Cardano (paymentUri)
import qualified Adatipd.Sql as Sql
import qualified Adatipd.WaiUtil as Wai (Application, responseHtml)
import qualified Codec.QRCode as Qr
import qualified Codec.QRCode.JuicyPixels as Qr
import qualified Data.Text.Lazy.Builder as TLB (toLazyText)
import qualified Data.Vector as Vector (fromList)
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
    { tsTitle :: Text
    , tsAmount :: Maybe Lovelace
      -- ^ A tip suggestion may have no specified amount.
    , tsQrImage :: Maybe QRImage
      -- ^ QR code creation may fail, so this is 'Maybe'.
    }

-- |
-- Create a tip suggestion from its parameters.
-- QR image will be generated automatically
-- and does not need to be given to this function.
mkTipSuggestion :: Text -> Maybe Lovelace -> Address -> TipSuggestion
mkTipSuggestion tsTitle tsAmount address =
  let
    -- High error correction leads to an enormous QR code.
    -- Medium should be good enough? I don’t really know.
    qrOptions = Qr.defaultQRCodeOptions Qr.M
    qrText = TLB.toLazyText (Cardano.paymentUri address tsAmount)
    tsQrImage = Qr.encodeText qrOptions Qr.Iso8859_1 qrText
  in
    TipSuggestion {..}

--------------------------------------------------------------------------------
-- Request handling

handleCreatorTipSuggestions
  :: Options -> Sql.Connection -> Nickname -> Wai.Application
handleCreatorTipSuggestions options sqlConn nickname request writeResponse =
  fetchCreatorTipSuggestions sqlConn nickname >>= \case
    Nothing ->
      handleNotFound options request writeResponse
    Just creatorTipSuggestions ->
      writeResponse $
        Wai.responseHtml status200 [] $
          renderCreatorTipSuggestions options creatorTipSuggestions

fetchCreatorTipSuggestions
  :: Sql.Connection -> Nickname -> IO (Maybe CreatorTipSuggestions)
fetchCreatorTipSuggestions sqlConn nickname = do
  creatorInfo <- fetchCreatorInfo sqlConn nickname

  -- TODO:
  -- If there are no tip suggestions,
  -- generate a default one with no suggested amount.

  let addr =
        Address
          "addr1qy7h4lxjsn7cz95gen79upe3lls6wlqn35m\
          \yruevx37utqal5fedzlcnfduhaqlqnyamuyt8apy\
          \6pfj6qu4fj8dmr4tsa3g6qz"

  case creatorInfo of
    Nothing -> pure Nothing
    Just ctsCreatorInfo ->
      pure . Just $
        CreatorTipSuggestions
          { ctsCreatorInfo
          , ctsTipAddress = addr
          , ctsTipSuggestions =
              Vector.fromList
                [ mkTipSuggestion "Koffie" (Just (Lovelace 1000000)) addr
                , mkTipSuggestion "Chips" (Just (Lovelace 2000000)) addr
                , mkTipSuggestion "Boterham" (Just (Lovelace 3000000)) addr
                , mkTipSuggestion "Mag ik een cola?" (Just (Lovelace 4000000)) addr
                , mkTipSuggestion "Support me!" Nothing addr ] }

renderCreatorTipSuggestions :: Options -> CreatorTipSuggestions -> Markup
renderCreatorTipSuggestions options@Options {..} CreatorTipSuggestions {..} =
  renderCreatorLayout options CreatorTipsTab ctsCreatorInfo $
    HH.section ! HA.class_ "creator-tip-suggestions" $ do
      HH.p ! HA.class_ "-tutorial" $ do
        "Feeling generous? Send "
        HH.strong $ HB.text (ciName ctsCreatorInfo)
        " a tip! 😍"
        HH.br
        "The amounts shown are suggestions; you may tip any amount."
        HH.br
        "Note that tips do not grant access to exclusive content."
        HH.br
        HH.text oInstanceTitle *> " will not charge you for sending tips."
      traverse_ (renderTipSuggestion ctsTipAddress) ctsTipSuggestions

renderTipSuggestion :: Address -> TipSuggestion -> Markup
renderTipSuggestion tipAddress TipSuggestion {..} =

  HH.article $ do

    HH.header ! HA.class_ "-header" $ do
      HH.h1 ! HA.class_ "-title" $
        HB.text tsTitle
      for_ tsAmount $
        \amount ->
          HH.p ! HA.class_ "-amount" $
            HB.textBuilder (formatAdaWithSymbol amount)

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

    HH.section ! HA.class_ "-address" $
      HB.text (formatBech32 tipAddress)
