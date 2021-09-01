-- SPDX-License-Identifier: AGPL-3.0-only

{-# LANGUAGE OverloadedStrings #-}

module Adatipd.Web
  ( handle
  ) where

import Adatipd.Options (Options (..))
import Adatipd.Web.CreatorPosts (handleCreatorPosts)
import Adatipd.Web.CreatorTiers (handleCreatorTiers)
import Adatipd.Web.CreatorTipSuggestions (handleCreatorTipSuggestions)
import Adatipd.Web.NotFound (handleNotFound)
import Adatipd.Nickname (Nickname)

import qualified Adatipd.Creator as Creator
import qualified Adatipd.Nickname as Nickname
import qualified Adatipd.Sql as Sql
import qualified Adatipd.WaiUtil as Wai

-- |
-- Handle an incoming HTTP request and write the HTTP response.
handle :: Options -> Sql.Connection -> Wai.Application
handle options sqlConn request writeResponse =
  case Wai.pathInfo request of

    -- Routes are defined by simple pattern matching.
    -- HTTP methods are distinguished by the handlers themselves,
    -- rather than pattern matching on them here.
    -- View patterns can be used to parse path components,
    -- such as in the examples below.

    (Nickname.parseUriComponent -> Right nickname) : _ ->
      handleCreator options sqlConn nickname request writeResponse

    _ ->
      handleNotFound options request writeResponse


-- | Handle any page behind the /~creatorid path.
handleCreator :: Options -> Sql.Connection -> Nickname -> Wai.Application
handleCreator options sqlConn nickname request writeResponse =
  Creator.fetchCreatorIdAndCurrentNickname sqlConn nickname >>= \case
    Nothing ->
      -- If no creator with this nickname exists, serve the generic not found
      -- page.
      handleNotFound options request writeResponse

    Just (_creatorId, currentNickname) | nickname /= currentNickname ->
      -- If the creator exists, but there is a newer nickname, redirect to the
      -- same url but with the new nickname. This loses the query string.
      writeResponse
        $ Wai.responseRedirectPermanently
        $ Nickname.formatUriComponent currentNickname
        : tail (Wai.pathInfo request)

    Just (creatorId, _) -> case tail (Wai.pathInfo request) of
      [] ->
        handleCreatorPosts options sqlConn creatorId request writeResponse

      ["tips"] ->
        handleCreatorTipSuggestions options sqlConn creatorId request writeResponse

      ["tiers"] ->
        handleCreatorTiers options sqlConn creatorId request writeResponse

      _ ->
        handleNotFound options request writeResponse
