-- SPDX-License-Identifier: AGPL-3.0-only

{-# LANGUAGE OverloadedStrings #-}

module Adatipd.Web
  ( handle
  ) where

import Adatipd.Nickname (Nickname)
import Adatipd.Web.AdminStatus (handleAdminStatus)
import Adatipd.Web.Context (Context (..), Options, makeContext)
import Adatipd.Web.CreatorPosts (handleCreatorPosts)
import Adatipd.Web.CreatorTiers (handleCreatorTiers)
import Adatipd.Web.CreatorTipSuggestions (handleCreatorTipSuggestions)
import Adatipd.Web.NotFound (handleNotFound)

import qualified Adatipd.Creator as Creator
import qualified Adatipd.Nickname as Nickname
import qualified Adatipd.Sql as Sql
import qualified Adatipd.WaiUtil as Wai

-- |
-- Handle an incoming HTTP request and write the HTTP response.
handle :: Options -> Sql.Connection -> Wai.Application
handle options sqlConn request writeResponse = do

  context <- makeContext options sqlConn request

  case Wai.pathInfo request of

    -- Routes are defined by simple pattern matching.
    -- HTTP methods are distinguished by the handlers themselves,
    -- rather than pattern matching on them here.
    -- View patterns can be used to parse path components,
    -- such as in the examples below.

    (Nickname.parseUriComponent -> Right nickname) : _ ->
      handleCreator context nickname request writeResponse

    -- TODO: Add access controls to these route.
    ["admin", "status"] ->
      handleAdminStatus context request writeResponse

    _ ->
      handleNotFound context request writeResponse


-- | Handle any page behind the /~creatorid path.
handleCreator :: Context -> Nickname -> Wai.Application
handleCreator context@Context {..} nickname request writeResponse =
  Creator.fetchCreatorIdAndCurrentNickname cSqlConn nickname >>= \case
    Nothing ->
      -- If no creator with this nickname exists, serve the generic not found
      -- page.
      handleNotFound context request writeResponse

    Just (_creatorId, currentNickname) | nickname /= currentNickname ->
      -- If the creator exists, but there is a newer nickname, redirect to the
      -- same url but with the new nickname. This loses the query string.
      writeResponse
        $ Wai.responseRedirectPermanently
        $ Nickname.formatUriComponent currentNickname
        : tail (Wai.pathInfo request)

    Just (creatorId, _) -> case tail (Wai.pathInfo request) of
      [] ->
        handleCreatorPosts context creatorId request writeResponse

      ["tips"] ->
        handleCreatorTipSuggestions context creatorId request writeResponse

      ["tiers"] ->
        handleCreatorTiers context creatorId request writeResponse

      _ ->
        handleNotFound context request writeResponse
