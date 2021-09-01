-- SPDX-License-Identifier: AGPL-3.0-only

{-# LANGUAGE OverloadedStrings #-}

module Adatipd.Web.CreatorLayout
  ( -- * Creator info
    CreatorInfo (..)
  , fetchCreatorInfo

    -- * Creator tabs
  , CreatorTab (..)

    -- * Creator layout
  , renderCreatorLayout
  ) where

import Adatipd.Nickname (Nickname)
import Adatipd.Options (Options)
import Adatipd.Web.Layout (renderLayout)
import Data.Functor.Contravariant ((>$<))
import Data.Text (Text)
import Text.Blaze (Markup, (!))

import qualified Adatipd.Nickname as Nickname (format, formatUriComponent)
import qualified Adatipd.Sql as Sql
import qualified Hasql.Decoders as SqlDec
import qualified Hasql.Encoders as SqlEnc
import qualified Text.Blaze as HB
import qualified Text.Blaze.Html5 as HH
import qualified Text.Blaze.Html5.Attributes as HA

--------------------------------------------------------------------------------
-- Creator info

data CreatorInfo =
  CreatorInfo
    { ciNickname :: Nickname
    , ciName :: Text
    , ciBiography :: Text }

fetchCreatorInfo :: Sql.Connection -> Nickname -> IO (Maybe CreatorInfo)
fetchCreatorInfo sqlConn nickname =

  Sql.runSession sqlConn $
    Sql.statement nickname $
      Sql.Statement
        "SELECT\n\
        \  creator_current_name(id),\n\
        \  creator_current_biography(id)\n\
        \FROM\n\
        \  creators, creator_nicknames\n\
        \WHERE\n\
        \  nickname = $1 \n\
        \  AND creators.id = creator_nicknames.creator_id"
        encodeNickname
        (SqlDec.rowMaybe decodeCreatorInfo)
        False

  where

    encodeNickname :: SqlEnc.Params Nickname
    encodeNickname =
      SqlEnc.param (SqlEnc.nonNullable (Nickname.format >$< SqlEnc.text))

    decodeCreatorInfo :: SqlDec.Row CreatorInfo
    decodeCreatorInfo = do
      let ciNickname = nickname
      ciName      <- SqlDec.column (SqlDec.nonNullable SqlDec.text)
      ciBiography <- SqlDec.column (SqlDec.nonNullable SqlDec.text)
      pure CreatorInfo {..}

--------------------------------------------------------------------------------
-- Creator tabs

data CreatorTab
  = CreatorPostsTab
  | CreatorTipsTab
  | CreatorTiersTab
  deriving stock (Eq)

renderCreatorTab :: CreatorTab -> Nickname -> CreatorTab -> String -> Markup
renderCreatorTab activeTab nickname thisTab label =
  HH.a
    ! HA.href (HH.toValue (creatorTabUri nickname thisTab))
    ! HA.class_ (if thisTab == activeTab then "-selected" else "")
    $ HB.string label

creatorTabUri :: Nickname -> CreatorTab -> Text
creatorTabUri nickname creatorTab =
  let
    suffix :: Text
    suffix =
      case creatorTab of
        CreatorPostsTab -> ""
        CreatorTipsTab -> "/tips"
        CreatorTiersTab -> "/tiers"
  in
    "/" <> Nickname.formatUriComponent nickname <> suffix

--------------------------------------------------------------------------------
-- Creator layout

renderCreatorLayout
  :: Options
  -> CreatorTab
  -> CreatorInfo
  -> Markup
  -> Markup
renderCreatorLayout options selectedTab CreatorInfo {..} content =

  renderLayout options ciName $ do

    HH.header ! HA.class_ "creator-banner" $ do
      HH.h1 ! HA.class_ "-name" $ HB.text ciName
      HH.p ! HA.class_ "-biography" $ HB.text ciBiography

    HH.nav ! HA.class_ "creator-tabs" $ do
      renderCreatorTab selectedTab ciNickname CreatorPostsTab "Posts"
      renderCreatorTab selectedTab ciNickname CreatorTipsTab "Tips"
      renderCreatorTab selectedTab ciNickname CreatorTiersTab "Tiers"

    content
