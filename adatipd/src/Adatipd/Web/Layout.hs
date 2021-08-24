{-# LANGUAGE OverloadedStrings #-}

module Adatipd.Web.Layout
  ( renderLayout
  ) where

import Adatipd.Options (Options (..))
import Data.Text (Text)
import Text.Blaze (Markup, (!))

import qualified Text.Blaze as HB
import qualified Text.Blaze.Html5 as HH
import qualified Text.Blaze.Html5.Attributes as HA

-- |
-- Surround the given title and content with HTML boilerplate.
-- This includes doctype, head elements, navigation bars, etc.
-- The options are used to decorate the page according to configuration.
renderLayout :: Options -> Text -> Markup -> Markup
renderLayout Options {..} title content = do
  HH.docType
  HH.meta ! HA.charset "utf-8"
  HH.title $ do
    HB.text title
    HB.text " — "
    HB.text oInstanceTitle
  content