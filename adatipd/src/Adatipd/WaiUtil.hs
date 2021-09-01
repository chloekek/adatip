-- SPDX-License-Identifier: AGPL-3.0-only

{-# LANGUAGE OverloadedStrings #-}

module Adatipd.WaiUtil
  ( module Network.Wai
  , responseHtml
  , responseRedirectPermanently
  ) where

import Network.Wai

import Data.Text (Text)
import Network.HTTP.Types.Header (ResponseHeaders)
import Network.HTTP.Types.Status (Status)
import Text.Blaze (Markup)
import Text.Blaze.Renderer.Utf8 (renderMarkupBuilder)

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Network.HTTP.Types.Status as Status

-- |
-- Create a response with an HTML body and a media type of @text/html@.
-- The HTML body is encoded as UTF-8.
responseHtml :: Status -> ResponseHeaders -> Markup -> Response
responseHtml status headers html =
  let
    headers' = ("Content-Type", "text/html") : headers
    html' = renderMarkupBuilder html
  in
    responseBuilder status headers' html'

-- |
-- Respond with 301 Moved Permanently to the given path, provided as segments,
-- inverse to Wai.pathInfo.
responseRedirectPermanently :: [Text] -> Response
responseRedirectPermanently pathInfo =
  let
    headers =
      [ ("Location", Text.encodeUtf8 $ "/" <> Text.intercalate "/" pathInfo) ]
  in
    responseLBS Status.movedPermanently301 headers ""
