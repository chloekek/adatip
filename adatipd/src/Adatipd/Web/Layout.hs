-- SPDX-License-Identifier: AGPL-3.0-only

{-# LANGUAGE OverloadedStrings #-}

module Adatipd.Web.Layout
  ( renderLayout
  ) where

import Adatipd.Web.Context (Context (..), Options (..))
import Data.Text (Text)
import Text.Blaze (Markup, (!))

import qualified Text.Blaze as HB
import qualified Text.Blaze.Html5 as HH
import qualified Text.Blaze.Html5.Attributes as HA

-- |
-- Surround the given title and content with HTML boilerplate.
-- This includes doctype, head elements, navigation bars, etc.
-- The options are used to decorate the page according to configuration.
renderLayout :: Context -> Text -> Markup -> Markup
renderLayout Context {..} title content = do
  let Options {..} = cOptions

  HH.docType

  HH.meta ! HA.charset "utf-8"

  HH.link ! HA.rel "stylesheet" ! HA.href "/static/stylesheet.css"

  HH.title $ do
    HB.text title
    HB.text " — "
    HB.text oInstanceTitle

  HH.header ! HA.class_ "page-header" $
    HH.a ! HA.class_ "-instance-title" ! HA.href "/" $
      HB.text oInstanceTitle

  HH.section ! HA.class_ "page-content" $
    content

  HH.section ! HA.class_ "page-footer" $
    HH.p $ do
      "Powered by "
      HH.a
        ! HA.href "https://github.com/chloekek/adatip"
        $ "Adatip"
      "."

      -- TODO: Do not display this in production.
      HB.string " Session identifier: "
      HB.string (show cSessionId)
      HB.string "."
