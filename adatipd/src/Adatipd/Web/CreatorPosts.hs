-- SPDX-License-Identifier: AGPL-3.0-only

{-# LANGUAGE OverloadedStrings #-}

module Adatipd.Web.CreatorPosts
  ( handleCreatorPosts
  ) where

import Adatipd.Web.CreatorLayout

import Adatipd.Creator (CreatorId)
import Adatipd.Options (Options)
import Data.Foldable (for_, traverse_)
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime, UTCTime, getCurrentTime, secondsToNominalDiffTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Vector (Vector)
import Network.HTTP.Types.Status (status200)
import Text.Blaze (Markup, (!))

import qualified Adatipd.Sql as Sql
import qualified Adatipd.WaiUtil as Wai (Application, responseHtml)
import qualified Data.Vector as Vector (fromList)
import qualified Text.Blaze as HB
import qualified Text.Blaze.Html5 as HH
import qualified Text.Blaze.Html5.Attributes as HA

--------------------------------------------------------------------------------
-- Retrieving posts

data CreatorPosts =
  CreatorPosts
    { chCreatorInfo :: CreatorInfo
    , chMostRecentPosts :: Vector Post }

data Post =
  Post
    { pTitle :: Text
    , pContent :: Text
    , pPublishedAbsolute :: UTCTime
    , pPublishedRelative :: NominalDiffTime
    , pAttachments :: Vector Attachment }

data Attachment
  = ImageAttachment -- TODO: This ctor should contain a URL or UUID or smth.
  | AudioAttachment -- TODO: This ctor should contain a URL or UUID or smth.
  | VideoAttachment -- TODO: This ctor should contain a URL or UUID or smth.

fetchCreatorPosts :: Sql.Connection -> CreatorId -> IO CreatorPosts
fetchCreatorPosts sqlConn creatorId = do
  creatorInfo <- fetchCreatorInfo sqlConn creatorId
  time <- getCurrentTime
  pure CreatorPosts
    { chCreatorInfo = creatorInfo
    , chMostRecentPosts =
        Vector.fromList
          [ Post
              "Vlog Friday #42!"
              "Alweer een nieuwe vlog."
              time
              (secondsToNominalDiffTime 3600)
              (Vector.fromList [ VideoAttachment ])
          , Post
              "Programming Podcast #3"
              "Vandaag praten we over Haskell."
              time
              (secondsToNominalDiffTime 7200)
              (Vector.fromList [ AudioAttachment ])
          , Post
              "Programming Podcast #2"
              "Vandaag praten we over Haskell."
              time
              (secondsToNominalDiffTime 290000)
              (Vector.fromList [ AudioAttachment ])
          , Post
              "Programming Podcast #1"
              "Vandaag praten we over Haskell."
              time
              (secondsToNominalDiffTime 3498300)
              (Vector.fromList [ AudioAttachment ]) ] }

--------------------------------------------------------------------------------
-- Request handling

handleCreatorPosts :: Options -> Sql.Connection -> CreatorId -> Wai.Application
handleCreatorPosts options sqlConn creatorId _request writeResponse = do
  creatorPosts <- fetchCreatorPosts sqlConn creatorId
  writeResponse $
    Wai.responseHtml status200 [] $
      renderCreatorPosts options creatorPosts

renderCreatorPosts :: Options -> CreatorPosts -> Markup
renderCreatorPosts options CreatorPosts {..} =
  renderCreatorLayout options CreatorPostsTab chCreatorInfo $
    HH.section ! HA.class_ "creator-posts" $
      traverse_ renderPost chMostRecentPosts

renderPost :: Post -> Markup
renderPost Post {..} =

  HH.article $ do

    HH.header ! HA.class_ "-header" $
      HH.h1 $ HB.text pTitle

    HH.section ! HA.class_ "-content" $
      HH.p $ HB.text pContent

    HH.section ! HA.class_ "-attachments" $
      for_ pAttachments $
        \case
          ImageAttachment -> HH.p "(todo: image attachment)"
          AudioAttachment -> HH.p "(todo: audio attachment)"
          VideoAttachment -> HH.p "(todo: video attachment)"

    HH.footer ! HA.class_ "-footer" $ do
      HH.time
        ! HA.datetime (HB.toValue (iso8601Show pPublishedAbsolute))
        ! HA.title (HB.toValue (iso8601Show pPublishedAbsolute))
        $ HB.string (show pPublishedRelative) *> " ago"
