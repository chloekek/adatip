-- SPDX-License-Identifier: AGPL-3.0-only

{-# LANGUAGE OverloadedStrings #-}

module Adatipd.Web.Context
  ( -- * Contexts
    Context (..)

    -- * Requests and responses
  , makeContext
  , SessionId
  , setSessionId

    -- * Working with session data
  , Session (..)
  , modifySession
  , flushSession

    -- * Convenient re-exports
  , Options (..)
  ) where

import Adatipd.Creator (CreatorId (..))
import Adatipd.Options (Options (..))
import Contravariant.Extras.Contrazip (contrazip2)
import Control.Category ((>>>))
import Data.ByteString (ByteString)
import Data.Functor.Contravariant ((>$<))
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.Maybe (fromMaybe)
import Data.UUID.Types (UUID)
import Web.Cookie (Cookies)

import qualified Adatipd.Sql as Sql
import qualified Adatipd.WaiUtil as Wai
import qualified Crypto.Hash.SHA256 as Sha256 (hashlazy)
import qualified Data.Binary.Builder as BSB (Builder, toLazyByteString)
import qualified Data.ByteString.Lazy as LBS (toStrict)
import qualified Data.UUID.Types as Uuid
import qualified Data.UUID.V4 as Uuid.V4 (nextRandom)
import qualified Hasql.Decoders as SqlDec
import qualified Hasql.Encoders as SqlEnc
import qualified Web.Cookie as Cookie

--------------------------------------------------------------------------------
-- Contexts

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
    , cSessionId :: SessionId
    , cSession :: Session
        -- ^ Session data at the start of the request handling.
    , cModifySession :: IORef (Maybe (Session -> Session))
        -- ^ How to modify session data at the end of the request handling.
        -- 'Nothing' is logically equivalent to @'Just' 'id'@,
        -- but in case of 'Nothing' we do not perform an upsert.
    }

--------------------------------------------------------------------------------
-- Requests and responses

-- |
-- Create a context for the current request.
makeContext :: Options -> Sql.Connection -> Wai.Request -> IO Context
makeContext cOptions cSqlConn request = do
  let cookies = getCookies request
  cSessionId <- getSessionId cookies
  cSession <- fetchSession cSqlConn cSessionId
  cModifySession <- newIORef Nothing
  pure Context {..}

-- |
-- Retrieve all cookies, as key–value pairs, from the request.
getCookies :: Wai.Request -> [(ByteString, ByteString)]
getCookies
  =   Wai.requestHeaders
  >>> lookup "Cookie"
  >>> maybe [] Cookie.parseCookies

-- |
-- Uniquely identifies a session.
newtype SessionId =
  SessionId UUID
  deriving newtype (Show)

-- |
-- Retrieve the session identifier from the @sessionId@ cookie.
-- If the cookie is malformed or missing,
-- a new session identifier is generated.
getSessionId :: Cookies -> IO SessionId
getSessionId
  =   lookup "sessionId"
  >>> (>>= Uuid.fromASCIIBytes)
  >>> maybe Uuid.V4.nextRandom pure
  >>> fmap SessionId

-- |
-- Set the @sessionId@ cookie to the session identifier from the context.
-- This must be done when sending a response,
-- and is handled by 'Adatipd.Web.handle'.
setSessionId :: Context -> Wai.Response -> Wai.Response
setSessionId Context {..} response =
  let
    Options {..} = cOptions
    SessionId sessionId = cSessionId

    setCookie :: BSB.Builder
    setCookie =
      Cookie.renderSetCookie
        Cookie.defaultSetCookie
          { Cookie.setCookieName     = "sessionId"
          , Cookie.setCookieValue    = Uuid.toASCIIBytes sessionId
          , Cookie.setCookiePath     = Just "/"
          , Cookie.setCookieMaxAge   = Just oSessionMaxAge
          , Cookie.setCookieHttpOnly = True -- Not accessible from JavaScript.
          , Cookie.setCookieSecure   = True -- Only send over TLS connection.
          }

    setCookie' :: ByteString
    setCookie' = LBS.toStrict (BSB.toLazyByteString setCookie)

  in
    Wai.mapResponseHeaders (("Set-Cookie", setCookie') :) response

--------------------------------------------------------------------------------
-- Working with session data

-- |
-- Because session identifiers grant access,
-- we do not want to store them directly in the database.
-- Instead, we store hashes of session identifiers.
hashSessionId :: SessionId -> ByteString
hashSessionId (SessionId sessionId) =
  Sha256.hashlazy (Uuid.toByteString sessionId)

-- |
-- A session keeps track of user-specific data across requests.
-- Sessions are tracked using session identifier cookies.
-- Note that this is not “tracking” in the mass surveillance sense,
-- but in the benign “functional cookies” sense.
--
-- Do not confuse with Hasql sessions. 😅
data Session =
  Session
    { sCreatorId :: Maybe CreatorId }
  deriving stock (Show)

-- |
-- Data for entirely new sessions.
-- Such sessions are typically not stored in the database
-- because they have not yet been modified.
newSession :: Session
newSession = Session Nothing

-- |
-- Schedule application of a function to the session,
-- to be flushed at the end of the request handling.
modifySession :: Context -> (Session -> Session) -> IO ()
modifySession Context {..} modify =
  modifyIORef cModifySession (Just . maybe modify (modify .))

-- |
-- Fetch a session from the database.
-- If there is no such session, returns 'newSession'.
fetchSession :: Sql.Connection -> SessionId -> IO Session
fetchSession sqlConn sessionId = do

  session <-
    Sql.runSession sqlConn $
      Sql.statement (hashSessionId sessionId) $
        Sql.Statement
          "SELECT\n\
          \  creator_id\n\
          \FROM\n\
          \  sessions\n\
          \WHERE\n\
          \  id_hash = $1"
        (SqlEnc.param (SqlEnc.nonNullable SqlEnc.bytea))
        (SqlDec.rowMaybe decodeSession)
        False

  pure $ fromMaybe newSession session

  where
    decodeSession :: SqlDec.Row Session
    decodeSession = do
      sCreatorId <- SqlDec.column (SqlDec.nullable (CreatorId <$> SqlDec.uuid))
      pure Session {..}

-- |
-- If the session was modified during the request handling,
-- write the new session data to the database.
-- You do not need to call this, as we call it
-- automatically at the end of the request handling.
flushSession :: Context -> IO ()
flushSession Context { cSqlConn, cSessionId, cSession, cModifySession } = do

  readIORef cModifySession >>= \case

    Nothing ->
      -- If the session was not modified then we do not need to upsert it.
      -- This also keeps empty sessions out of the database.
      pure ()

    Just modify -> do

      -- Apply the session modifications to the old session.
      let Session {..} = modify cSession

      -- Upsert the new session into the database.
      Sql.runSession cSqlConn $
        Sql.statement (hashSessionId cSessionId, sCreatorId) $
          Sql.Statement
            "INSERT INTO sessions (id_hash, created, creator_id)\n\
            \VALUES ($1, now(), $2)\n\
            \ON CONFLICT (id_hash)\n\
            \  DO UPDATE\n\
            \    SET creator_id = $2"
            encodeParams
            SqlDec.noResult
            False

  where
    encodeParams :: SqlEnc.Params (ByteString, Maybe CreatorId)
    encodeParams =
      contrazip2
        (SqlEnc.param (SqlEnc.nonNullable SqlEnc.bytea))
        (SqlEnc.param (SqlEnc.nullable (getCreatorId >$< SqlEnc.uuid)))
