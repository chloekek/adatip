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

import Adatipd.Cardano (Address, Lovelace)
import Adatipd.Nickname (Nickname)
import Adatipd.Options (Options (..))
import Adatipd.Web.Layout (renderLayout)
import Adatipd.Web.NotFound (handleNotFound)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Vector (Vector)
import Network.HTTP.Types.Status (status200)
import Text.Blaze (Markup)

import qualified Adatipd.Nickname as Nickname (format)
import qualified Adatipd.WaiUtil as Wai (Application, responseHtml)
import qualified Data.Vector as Vector (empty)
import qualified Text.Blaze as HB
import qualified Text.Blaze.Html5 as HH

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
        , chTipSuggestions = Vector.empty
        , chTotalPosts = Vector.empty
        , chMostRecentPosts = Vector.empty
        }

-- |
-- Render a creator’s home page as HTML.
renderCreatorHome :: Options -> CreatorHome -> Markup
renderCreatorHome options CreatorHome {..} =
  renderLayout options chName $ do
    HH.h1 $ HB.text chName
    HH.p $ HB.text chBiography

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
