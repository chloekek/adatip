-- SPDX-License-Identifier: AGPL-3.0-only

{-# LANGUAGE OverloadedStrings #-}

module Adatipd.Web.Context
  ( Context (..)
  , makeContext
  , setSessionId

    -- * Convenient re-exports
  , Options (..)
  ) where

import Adatipd.Options (Options (..))
import Control.Category ((>>>))
import Data.ByteString (ByteString)
import Data.UUID.Types (UUID)
import Web.Cookie (Cookies)

import qualified Adatipd.Sql as Sql (Connection)
import qualified Adatipd.WaiUtil as Wai
import qualified Data.Binary.Builder as BSB (Builder, toLazyByteString)
import qualified Data.ByteString.Lazy as LBS (toStrict)
import qualified Data.UUID.Types as Uuid (fromASCIIBytes, toASCIIBytes)
import qualified Data.UUID.V4 as Uuid.V4 (nextRandom)
import qualified Web.Cookie as Cookie

-- |
-- Information that is available within any request handler.
-- This information is made available by the routing code in "Adatipd.Web".
--
-- You can just pass this around throughout the request handling code.
-- Even if you do not need all the fields, just passing this around is easier.
data Context =
  Context
    { cOptions :: Options
    , cSqlConn :: Sql.Connection
    , cSessionId :: UUID }

-- |
-- Create a context for the current request.
makeContext :: Options -> Sql.Connection -> Wai.Request -> IO Context
makeContext cOptions cSqlConn request = do
  let cookies = getCookies request
  cSessionId <- getSessionId cookies
  pure Context {..}

-- |
-- Retrieve all cookies, as key–value pairs, from the request.
getCookies :: Wai.Request -> [(ByteString, ByteString)]
getCookies
  =   Wai.requestHeaders
  >>> lookup "Cookie"
  >>> maybe [] Cookie.parseCookies

-- |
-- Retrieve the session identifier from the @sessionId@ cookie.
-- If the cookie is malformed or missing,
-- a new session identifier is generated.
getSessionId :: Cookies -> IO UUID
getSessionId
  =   lookup "sessionId"
  >>> (>>= Uuid.fromASCIIBytes)
  >>> maybe Uuid.V4.nextRandom pure

-- |
-- Set the @sessionId@ cookie to the session identifier from the context.
-- This must be done when sending a response,
-- and is handled by 'Adatipd.Web.handle'.
setSessionId :: Context -> Wai.Response -> Wai.Response
setSessionId Context {..} response =
  let
    Options {..} = cOptions

    setCookie :: BSB.Builder
    setCookie =
      Cookie.renderSetCookie
        Cookie.defaultSetCookie
          { Cookie.setCookieName     = "sessionId"
          , Cookie.setCookieValue    = Uuid.toASCIIBytes cSessionId
          , Cookie.setCookiePath     = Just "/"
          , Cookie.setCookieMaxAge   = Just oSessionMaxAge
          , Cookie.setCookieHttpOnly = True -- Not accessible from JavaScript.
          , Cookie.setCookieSecure   = True -- Only send over TLS connection.
          }

    setCookie' :: ByteString
    setCookie' = LBS.toStrict (BSB.toLazyByteString setCookie)

  in
    Wai.mapResponseHeaders (("Set-Cookie", setCookie') :) response
