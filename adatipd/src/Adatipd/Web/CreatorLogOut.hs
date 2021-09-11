-- SPDX-License-Identifier: AGPL-3.0-only

{-# LANGUAGE OverloadedStrings #-}

module Adatipd.Web.CreatorLogOut
  ( handleCreatorLogOut
  ) where

import Adatipd.Web.Context (Context, Session (..))
import Adatipd.Web.Layout (renderLayout)
import Network.HTTP.Types.Status (status200, seeOther303)
import Text.Blaze (Markup, (!))

import qualified Adatipd.WaiUtil as Wai
import qualified Adatipd.Web.Context as Context (modifySession)
import qualified Text.Blaze.Html5 as HH
import qualified Text.Blaze.Html5.Attributes as HA

handleCreatorLogOut :: Context -> Wai.Application
handleCreatorLogOut context request writeResponse =
  case Wai.requestMethod request of

    "GET" ->
      writeResponse $
        Wai.responseHtml status200 [] $
          renderCreatorLogOut context

    "POST" -> do
      performLogOut context request writeResponse

    _ ->
      error "handleMethodNotAllowed context request writeResponse"

renderCreatorLogOut :: Context -> Markup
renderCreatorLogOut context =
  renderLayout context "Log out" $
    HH.form
      ! HA.class_ "creator-log-out"
      ! HA.method "post"
      ! HA.action "/creator/log-out"
      $ do
          HH.p "Are you sure you want to log out?"
          HH.button ! HA.type_ "submit" $ "Log out"

performLogOut :: Context -> Wai.Application
performLogOut context _request writeResponse = do

  Context.modifySession context $
    \session -> session { sCreatorId = Nothing }

  writeResponse $
    -- 303 See Other redirects using the GET method.
    -- This is exactly what we want for logging out.
    Wai.responseRedirect seeOther303 []
