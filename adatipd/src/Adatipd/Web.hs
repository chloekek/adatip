{-# LANGUAGE OverloadedStrings #-}

module Adatipd.Web
  ( handle
  ) where

import Adatipd.Options (Options (..))
import Adatipd.Web.CreatorPosts (handleCreatorPosts)
import Adatipd.Web.CreatorTiers (handleCreatorTiers)
import Adatipd.Web.CreatorTipSuggestions (handleCreatorTipSuggestions)
import Adatipd.Web.NotFound (handleNotFound)

import qualified Adatipd.Nickname as Nickname (parseUriComponent)
import qualified Adatipd.Sql as Sql
import qualified Network.Wai as Wai (Application, pathInfo)

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

    [Nickname.parseUriComponent -> Right nickname] ->
      handleCreatorPosts options sqlConn nickname request writeResponse

    [Nickname.parseUriComponent -> Right nickname, "tips"] ->
      handleCreatorTipSuggestions options sqlConn nickname request writeResponse

    [Nickname.parseUriComponent -> Right nickname, "tiers"] ->
      handleCreatorTiers options sqlConn nickname request writeResponse

    _ ->
      handleNotFound options request writeResponse
