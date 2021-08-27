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
  , Attachment (..)
  ) where

import Adatipd.Cardano (Address (..), Lovelace (..), formatAda, formatBech32)
import Adatipd.Nickname (Nickname)
import Adatipd.Options (Options (..))
import Adatipd.Web.Layout (renderLayout)
import Adatipd.Web.NotFound (handleNotFound)
import Data.Foldable (for_, traverse_)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Vector (Vector)
import Network.HTTP.Types.Status (status200)
import Text.Blaze (Markup, (!))

import qualified Adatipd.Nickname as Nickname (format)
import qualified Adatipd.Sql as Sql
import qualified Adatipd.WaiUtil as Wai (Application, responseHtml)
import qualified Codec.QRCode as Qr
import qualified Codec.QRCode.JuicyPixels as Qr
import qualified Data.Vector as Vector (fromList)
import qualified Hasql.Decoders as SqlDec
import qualified Hasql.Encoders as SqlEnc
import qualified Text.Blaze as HB
import qualified Text.Blaze.Html5 as HH
import qualified Text.Blaze.Html5.Attributes as HA

--------------------------------------------------------------------------------
-- Interface

-- |
-- Fetch the creator with the given name and display their home page.
-- The data types in this module tell you what that looks like.
handleCreatorHome :: Options -> Sql.Connection -> Nickname -> Wai.Application
handleCreatorHome options sqlConn nickname request writeResponse =
  fetchCreatorHome sqlConn nickname >>= \case
    Nothing ->
      handleNotFound options request writeResponse
    Just creatorHome ->
      writeResponse $
        Wai.responseHtml status200 [] $
          renderCreatorHome options creatorHome

-- |
-- Fetch the data to display for a creator with a given nickname.
-- If the creator does not exist, this function returns 'Nothing'.
fetchCreatorHome :: Sql.Connection -> Nickname -> IO (Maybe CreatorHome)
fetchCreatorHome sqlConn nickname = do

  -- This is just to demo SQL connectivity for now.
  -- In the future we will get actual user info from the database.
  biography <-
    Sql.runSession sqlConn $
      Sql.statement () $
        Sql.Statement
          "SELECT version()"
          SqlEnc.noParams
          (SqlDec.singleRow (SqlDec.column (SqlDec.nonNullable SqlDec.text)))
          False

  if Nickname.format nickname == "henkdevries"
    then pure (Just (henkdevries biography))
    else pure Nothing

  where
    henkdevries :: Text -> CreatorHome
    henkdevries biography =
      CreatorHome
        { chNickname = nickname
        , chName = "Henk de Vries"
        , chBiography = biography
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
        , chTotalPosts =
            Vector.fromList
              [ TotalPosts "Henk Tier 1" 100
              , TotalPosts "Henk Tier 2"   5 ]
        , chMostRecentPosts =
            Vector.fromList
              [ Post "Vlog Friday #42!" "Alweer een nieuwe vlog."
                  (Vector.fromList [ VideoAttachment ])
              , Post "Programming Podcast #1" "Vandaag praten we over Haskell."
                  (Vector.fromList [ AudioAttachment ]) ] }

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

    HH.section $ do
      HH.h1 "Exclusive content"
      traverse_ renderPost chMostRecentPosts

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

-- |
-- Render a single post.
renderPost :: Post -> Markup
renderPost Post {..} =

  HH.article ! HA.class_ "post" $ do

    HH.header $ do
      HH.h1 ! HA.class_ "title" $
        HB.text pTitle

    HH.section ! HA.class_ "content" $
      HH.p $ HB.text pContent

    HH.section ! HA.class_ "attachments" $
      for_ pAttachments $
        \case
          ImageAttachment -> HH.p "(todo: image attachment)"
          AudioAttachment -> HH.p "(todo: audio attachment)"
          VideoAttachment -> HH.p "(todo: video attachment)"

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
    , pContent :: Text
    , pAttachments :: Vector Attachment }

data Attachment
  = ImageAttachment -- TODO: This ctor should contain a URL or UUID or smth.
  | AudioAttachment -- TODO: This ctor should contain a URL or UUID or smth.
  | VideoAttachment -- TODO: This ctor should contain a URL or UUID or smth.
