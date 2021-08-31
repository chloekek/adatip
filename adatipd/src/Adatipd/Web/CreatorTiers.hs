-- SPDX-License-Identifier: AGPL-3.0-only

{-# LANGUAGE OverloadedStrings #-}

module Adatipd.Web.CreatorTiers
  ( handleCreatorTiers
  ) where

import Adatipd.Web.CreatorLayout

import Adatipd.Cardano (Lovelace (..), formatAdaWithSymbol)
import Adatipd.Nickname (Nickname)
import Adatipd.Options (Options (..))
import Adatipd.Web.NotFound (handleNotFound)
import Data.Foldable (traverse_)
import Data.Int (Int64)
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
import qualified Text.Blaze.Internal as HB (textBuilder)

--------------------------------------------------------------------------------
-- Retrieving tiers

data CreatorTiers =
  CreatorTiers
    { ctCreatorInfo :: CreatorInfo
    , ctTiers :: Vector Tier }

data Tier =
  Tier
    { tName :: Text
    , tDescription :: Text
    , tCost :: Lovelace
    , tPosts :: Int64 }

fetchCreatorTiers :: Sql.Connection -> Nickname -> IO (Maybe CreatorTiers)
fetchCreatorTiers sqlConn nickname = do
  creatorInfo <- fetchCreatorInfo sqlConn nickname
  case creatorInfo of
    Nothing -> pure Nothing
    Just ctCreatorInfo ->
      pure . Just $
        CreatorTiers
          { ctCreatorInfo
          , ctTiers =
              Vector.fromList
                [ Tier "Henk Tier #1" lipsum (Lovelace 1_000_000) 40
                , Tier "Henk Tier #2" lipsum (Lovelace 2_000_000) 35
                , Tier "Henk Tier #3" lipsum (Lovelace 3_500_000) 6 ] }

lipsum :: Text
lipsum =
  "Lorem ipsum dolor sit amet, consectetur adipiscing elit. \
  \Sed id lobortis odio. Pellentesque hendrerit arcu orci, \
  \et malesuada enim tristique eu. Aenean nec luctus lectus, \
  \non feugiat orci. Nunc condimentum consectetur arcu, \
  \eget vulputate neque euismod a. Ut eu erat id mi lobortis ornare. \
  \Ut lorem odio, posuere vel varius vitae, facilisis non nibh. \
  \Ut tortor leo, cursus ac nulla vitae, facilisis pharetra tellus. \
  \Proin semper efficitur eros. Morbi finibus placerat eleifend. \
  \Vivamus vulputate sollicitudin scelerisque. Morbi eget tortor scelerisque, \
  \volutpat sem nec, mattis urna. Sed rutrum libero laoreet sapien egestas, \
  \in porta eros interdum."

--------------------------------------------------------------------------------
-- Request handling

handleCreatorTiers :: Options -> Sql.Connection -> Nickname -> Wai.Application
handleCreatorTiers options sqlConn nickname request writeResponse =
  fetchCreatorTiers sqlConn nickname >>= \case
    Nothing ->
      handleNotFound options request writeResponse
    Just creatorTiers ->
      writeResponse $
        Wai.responseHtml status200 [] $
          renderCreatorTiers options creatorTiers

renderCreatorTiers :: Options -> CreatorTiers -> Markup
renderCreatorTiers options CreatorTiers {..} =
  renderCreatorLayout options CreatorTiersTab ctCreatorInfo $
    HH.section ! HA.class_ "creator-tiers" $
      traverse_ renderTier ctTiers

renderTier :: Tier -> Markup
renderTier Tier {..} =

  HH.article $ do

    HH.header ! HA.class_ "-header" $
      HH.h1 $ HB.text tName

    HH.section ! HA.class_ "-description" $
      HH.p $ HB.text tDescription

    HH.footer ! HA.class_ "-footer" $ do

      HH.span ! HA.class_ "-subscribers" $
        HB.string (show tPosts) *> " subscribers"

      HH.a
        ! HA.class_ "-subscribe"
        ! HA.href "javascript:void(0)"
        $ "Subscribe for " *> HB.textBuilder (formatAdaWithSymbol tCost)
