-- SPDX-License-Identifier: AGPL-3.0-only

{-# LANGUAGE OverloadedStrings #-}

module Adatipd.Web.CreatorPosts
  ( handleCreatorPosts
  ) where

import Adatipd.Web.CreatorLayout

import Adatipd.Creator (CreatorId, encodeCreatorId)
import Adatipd.Web.Context (Context (..))
import Data.Foldable (for_, traverse_)
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Vector (Vector)
import Network.HTTP.Types.Status (status200)
import Text.Blaze (Markup, (!))

import qualified Adatipd.Sql as Sql
import qualified Adatipd.WaiUtil as Wai (Application, responseHtml)
import qualified Data.Vector as Vector (fromList)
import qualified Hasql.Decoders as SqlDec
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

  chCreatorInfo <- fetchCreatorInfo sqlConn creatorId

  currentTime <- getCurrentTime

  posts <-
    Sql.runSession sqlConn $
      Sql.statement creatorId $
        Sql.Statement
          "SELECT\n\
          \  post_current_title(id),\n\
          \  post_current_content(id),\n\
          \  post_first_published(id)\n\
          \FROM\n\
          \  posts\n\
          \WHERE\n\
          \  creator_id = $1\n\
          \  AND post_current_visibility(id)\n\
          \ORDER BY\n\
          \  post_first_published(id) DESC"
          encodeCreatorId
          (SqlDec.rowVector (decodePost currentTime))
          False

  pure
    CreatorPosts
      { chCreatorInfo
      , chMostRecentPosts = posts }

  where

    decodePost :: UTCTime -> SqlDec.Row Post
    decodePost currentTime = do
      pTitle <- SqlDec.column (SqlDec.nonNullable SqlDec.text)
      pContent <- SqlDec.column (SqlDec.nonNullable SqlDec.text)
      pPublishedAbsolute <- SqlDec.column (SqlDec.nonNullable SqlDec.timestamptz)
      let pPublishedRelative = currentTime `diffUTCTime` pPublishedAbsolute
      let pAttachments = Vector.fromList []
      pure Post {..}

--------------------------------------------------------------------------------
-- Request handling

handleCreatorPosts :: Context -> CreatorId -> Wai.Application
handleCreatorPosts context@Context {..} creatorId _request writeResponse = do
  creatorPosts <- fetchCreatorPosts cSqlConn creatorId
  writeResponse $
    Wai.responseHtml status200 [] $
      renderCreatorPosts context creatorPosts

renderCreatorPosts :: Context -> CreatorPosts -> Markup
renderCreatorPosts context CreatorPosts {..} =
  renderCreatorLayout context CreatorPostsTab chCreatorInfo $
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
