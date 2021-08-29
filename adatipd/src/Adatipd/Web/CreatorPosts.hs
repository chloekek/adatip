{-# LANGUAGE OverloadedStrings #-}

module Adatipd.Web.CreatorPosts
  ( handleCreatorPosts
  ) where

import Adatipd.Web.CreatorLayout

import Adatipd.Nickname (Nickname)
import Adatipd.Options (Options)
import Adatipd.Web.NotFound (handleNotFound)
import Data.Foldable (for_, traverse_)
import Data.Text (Text)
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
    , pAttachments :: Vector Attachment }

data Attachment
  = ImageAttachment -- TODO: This ctor should contain a URL or UUID or smth.
  | AudioAttachment -- TODO: This ctor should contain a URL or UUID or smth.
  | VideoAttachment -- TODO: This ctor should contain a URL or UUID or smth.

fetchCreatorPosts :: Sql.Connection -> Nickname -> IO (Maybe CreatorPosts)
fetchCreatorPosts sqlConn nickname = do
  creatorInfo <- fetchCreatorInfo sqlConn nickname
  case creatorInfo of
    Nothing -> pure Nothing
    Just chCreatorInfo ->
      pure . Just $
        CreatorPosts
          { chCreatorInfo
          , chMostRecentPosts =
              Vector.fromList
                [ Post "Vlog Friday #42!" "Alweer een nieuwe vlog."
                    (Vector.fromList [ VideoAttachment ])
                , Post "Programming Podcast #3" "Vandaag praten we over Haskell."
                    (Vector.fromList [ AudioAttachment ])
                , Post "Programming Podcast #2" "Vandaag praten we over Haskell."
                    (Vector.fromList [ AudioAttachment ])
                , Post "Programming Podcast #1" "Vandaag praten we over Haskell."
                    (Vector.fromList [ AudioAttachment ]) ] }

--------------------------------------------------------------------------------
-- Request handling

handleCreatorPosts :: Options -> Sql.Connection -> Nickname -> Wai.Application
handleCreatorPosts options sqlConn nickname request writeResponse =
  fetchCreatorPosts sqlConn nickname >>= \case
    Nothing ->
      handleNotFound options request writeResponse
    Just creatorPosts ->
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
