{-# LANGUAGE OverloadedStrings #-}

module Adatipd.Web.CreatorHome
  ( -- * Interface
    handleCreatorHome
  , fetchCreatorHome
  , renderCreatorHome

    -- * Data types
  , CreatorHome (..)
  , TipSuggestion (..)
  , TotalPosts (..)
  , Post (..)
  ) where

import Adatipd.Cardano (Address (..), Lovelace (..), formatAda, formatBech32)
import Adatipd.Nickname (Nickname)
import Adatipd.Options (Options (..))
import Adatipd.Web.Layout (renderLayout)
import Adatipd.Web.NotFound (handleNotFound)
import Data.Foldable (traverse_)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Vector (Vector)
import Network.HTTP.Types.Status (status200)
import Text.Blaze (Markup, (!))

import qualified Adatipd.Nickname as Nickname (format)
import qualified Adatipd.WaiUtil as Wai (Application, responseHtml)
import qualified Codec.QRCode as Qr
import qualified Codec.QRCode.JuicyPixels as Qr
import qualified Data.Vector as Vector (empty, fromList)
import qualified Text.Blaze as HB
import qualified Text.Blaze.Html5 as HH
import qualified Text.Blaze.Html5.Attributes as HA

--------------------------------------------------------------------------------
-- Interface

-- |
-- Fetch the creator with the given name and display their home page.
-- The data types in this module tell you what that looks like.
handleCreatorHome :: Options -> Nickname -> Wai.Application
handleCreatorHome options nickname request writeResponse =
  fetchCreatorHome nickname >>= \case
    Nothing ->
      handleNotFound options request writeResponse
    Just creatorHome ->
      writeResponse $
        Wai.responseHtml status200 [] $
          renderCreatorHome options creatorHome

-- |
-- Fetch the data to display for a creator with a given nickname.
-- If the creator does not exist, this function returns 'Nothing'.
fetchCreatorHome :: Nickname -> IO (Maybe CreatorHome)
fetchCreatorHome nickname =
  if Nickname.format nickname == "henkdevries"
    then pure (Just henkdevries)
    else pure Nothing
  where
    henkdevries :: CreatorHome
    henkdevries =
      CreatorHome
        { chNickname = nickname
        , chName = "Henk de Vries"
        , chBiography = "Ik ben Henk de Vries!"
        , chTipSuggestions =
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
                           \6pfj6qu4fj8dmr4ts6ednhv") ]
        , chTotalPosts = Vector.empty
        , chMostRecentPosts = Vector.empty
        }

-- |
-- Render a creator’s home page as HTML.
renderCreatorHome :: Options -> CreatorHome -> Markup
renderCreatorHome options CreatorHome {..} =
  renderLayout options chName $ do

    HH.header $ do
      HH.h1 $ HB.text chName
      HH.p $ HB.text chBiography

    HH.section $ do
      HH.h1 "Tip suggestions"
      traverse_ renderTipSuggestion chTipSuggestions

-- |
-- Render a tip suggestion, with nice QR code.
-- The QR code and address are hidden until
-- the user interacts with the tip suggestion.
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

  HH.article ! HA.class_ "tip-suggestion" $

    HH.details $ do

      HH.summary $
        HH.header $ do
          HH.div ! HA.class_ "amount" $
            HB.string (formatAda tsAmount)
          HH.h1 ! HA.class_ "title" $
            HB.text tsTitle

      case qrImagePng of
        Nothing ->
          HB.stringComment
            "Unfortunately no QR code could \
            \be generated for this address."
        Just qrImagePng' ->
          HH.section ! HA.class_ "qr-code" $
            HH.img ! HA.src (HB.lazyTextValue qrImagePng')

      HH.section ! HA.class_ "address" $
        HB.text formattedAddress

--------------------------------------------------------------------------------
-- Data types

-- |
-- Information shown on a creator’s home page.
-- It includes information about the creator and their posts.
data CreatorHome =
  CreatorHome
    { chNickname :: Nickname
    , chName :: Text
    , chBiography :: Text
    , chTipSuggestions :: Vector TipSuggestion
    , chTotalPosts :: Vector TotalPosts
    , chMostRecentPosts :: Vector Post }

-- |
-- A tip suggested by a creator, shown on their home page.
-- Includes a suggested amount and a Cardano address to send the tip to.
-- Of course a tipper is free to deviate from the suggested amount.
data TipSuggestion =
  TipSuggestion
    { tsTitle :: Text
    , tsAmount :: Lovelace
    , tsAddress :: Address }

-- |
-- How many posts are available to subscribers for a given tier.
-- This is shown on a creator’s home page to give prospect subscribers
-- an idea of what they will get if they subscribe.
data TotalPosts =
  TotalPosts
    { tpTierName :: Text
    , tpPostCount :: Int64 }

data Post =
  Post
    { pTitle :: Text
    , pSummary :: Text
    , pContent :: Vector Media
    }

data Media
