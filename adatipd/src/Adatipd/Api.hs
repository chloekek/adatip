module Adatipd.Api
  ( Api
  ) where

import Adatipd.Cardano (Address, Lovelace)
import Adatipd.Nickname (Nickname)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Vector (Vector)
import Servant.API (type (:>), Capture, Get, JSON)

type Api =
  Capture "nickname" Nickname :> Get '[JSON] CreatorHome

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

data Post
