-- SPDX-License-Identifier: AGPL-3.0-only

{-# LANGUAGE OverloadedStrings #-}

module Adatipd.Web.NotFound
  ( handleNotFound
  , renderNotFound
  ) where

import Adatipd.Options (Options)
import Adatipd.Web.Layout (renderLayout)
import Network.HTTP.Types.Status (status404)
import Text.Blaze (Markup)

import qualified Adatipd.WaiUtil as Wai (Application, responseHtml)
import qualified Text.Blaze as HB
import qualified Text.Blaze.Html5 as HH

-- |
-- Respond with a “404 Not Found” page.
-- This also sets the status code to 404.
handleNotFound :: Options -> Wai.Application
handleNotFound options _request writeResponse =
  writeResponse $
    Wai.responseHtml status404 [] $
      renderNotFound options

-- |
-- Render a “404 Not Found” page.
renderNotFound :: Options -> Markup
renderNotFound options =
  let title = "Page not found" in
  renderLayout options title $
    HH.h1 $ HB.text title
