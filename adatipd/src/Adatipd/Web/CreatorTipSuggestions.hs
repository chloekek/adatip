{-# LANGUAGE OverloadedStrings #-}

module Adatipd.Web.CreatorTipSuggestions
  ( handleCreatorTipSuggestions
  ) where

import Adatipd.Web.CreatorLayout

import Adatipd.Cardano (Address (..), Lovelace (..), formatAda, formatBech32)
import Adatipd.Nickname (Nickname)
import Adatipd.Options (Options)
import Adatipd.Web.NotFound (handleNotFound)
import Data.Foldable (traverse_)
import Data.Text (Text)
import Data.Vector (Vector)
import Network.HTTP.Types.Status (status200)
import Text.Blaze (Markup, (!))

import qualified Adatipd.Sql as Sql
import qualified Adatipd.WaiUtil as Wai (Application, responseHtml)
import qualified Codec.QRCode as Qr
import qualified Codec.QRCode.JuicyPixels as Qr
import qualified Data.Vector as Vector (fromList)
import qualified Text.Blaze as HB
import qualified Text.Blaze.Html5 as HH
import qualified Text.Blaze.Html5.Attributes as HA

--------------------------------------------------------------------------------
-- Retrieving tip suggestions

data CreatorTipSuggestions =
  CreatorTipSuggestions
    { ctsCreatorInfo :: CreatorInfo
    , ctsTipSuggestions :: Vector TipSuggestion }

data TipSuggestion =
  TipSuggestion
    { tsTitle :: Text
    , tsAmount :: Lovelace
    , tsAddress :: Address }

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
  case creatorInfo of
    Nothing -> pure Nothing
    Just ctsCreatorInfo ->
      pure . Just $
        CreatorTipSuggestions
          { ctsCreatorInfo
          , ctsTipSuggestions =
              Vector.fromList
                [ TipSuggestion
                    "Koffie"
                    (Lovelace 1000000)
                    (Address "addr1qy7h4lxjsn7cz95gen79upe3lls6wlqn35m\
                              \yruevx37utqal5fedzlcnfduhaqlqnyamuyt8apy\
                              \6pfj6qu4fj8dmr4tsa3g6qz")
                , TipSuggestion
                    "Chips"
                    (Lovelace 2000000)
                    (Address "addr1q8g690sm5t32j3xk24456dmeaye9yvv5t60\
                              \s6wtqry58p7dl5fedzlcnfduhaqlqnyamuyt8apy\
                              \6pfj6qu4fj8dmr4ts6ednhv")
                , TipSuggestion
                    "Boterham"
                    (Lovelace 3000000)
                    (Address "addr1q8g690sm5t32j3xk24456dmeaye9yvv5t60\
                              \s6wtqry58p7dl5fedzlcnfduhaqlqnyamuyt8apy\
                              \6pfj6qu4fj8dmr4ts6ednhv")
                , TipSuggestion
                    "Mag ik een cola?"
                    (Lovelace 4000000)
                    (Address "addr1q8g690sm5t32j3xk24456dmeaye9yvv5t60\
                              \s6wtqry58p7dl5fedzlcnfduhaqlqnyamuyt8apy\
                              \6pfj6qu4fj8dmr4ts6ednhv") ] }

renderCreatorTipSuggestions :: Options -> CreatorTipSuggestions -> Markup
renderCreatorTipSuggestions options CreatorTipSuggestions {..} =
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
      traverse_ renderTipSuggestion ctsTipSuggestions

renderTipSuggestion :: TipSuggestion -> Markup
renderTipSuggestion TipSuggestion {..} = do

  -- This is the standard format for Cardano addresses.
  -- All wallets with QR code functionality should support it.
  let formattedAddress =
        formatBech32 tsAddress

  -- High error connection leads to an enormous QR code.
  -- Medium should be good enough? I don’t really know.
  let qrOptions = Qr.defaultQRCodeOptions Qr.M
      qrImage = Qr.encodeText qrOptions Qr.Iso8859_1 formattedAddress
      qrImagePng = Qr.toPngDataUrlT 4 8 <$> qrImage

  HH.article $ do

    HH.header ! HA.class_ "-header" $ do
      HH.h1 ! HA.class_ "-title" $ HB.text tsTitle
      HH.p ! HA.class_ "-amount" $ HB.string (formatAda tsAmount)

    case qrImagePng of
      Nothing ->
        HB.stringComment
          "Unfortunately no QR code could \
          \be generated for this address."
      Just qrImagePng' ->
        HH.img
          ! HA.class_ "-qr-code"
          ! HA.src (HB.lazyTextValue qrImagePng')

    HH.section ! HA.class_ "-address" $
      HB.text formattedAddress
