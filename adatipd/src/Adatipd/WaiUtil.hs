-- SPDX-License-Identifier: AGPL-3.0-only

{-# LANGUAGE OverloadedStrings #-}

module Adatipd.WaiUtil
  ( module Network.Wai
  , responseHtml
  ) where

import Network.Wai

import Network.HTTP.Types.Header (ResponseHeaders)
import Network.HTTP.Types.Status (Status)
import Text.Blaze (Markup)
import Text.Blaze.Renderer.Utf8 (renderMarkupBuilder)

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
