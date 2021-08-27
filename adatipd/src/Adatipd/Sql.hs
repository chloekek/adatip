module Adatipd.Sql
  ( -- * Connections
    Connection
  , withConnection

    -- * Queries
  , Statement (..)
  , runSession
  , Hasql.sql
  , Hasql.statement

    -- * Exceptions
  , ConnectionException (..)
  , QueryException (..)
  ) where

import Control.Exception (Exception, bracket, throwIO)
import Hasql.Connection (Connection)
import Hasql.Session (Session)
import Hasql.Statement (Statement (..))

import qualified Data.ByteString as BS (ByteString)
import qualified Hasql.Connection as Hasql (ConnectionError, acquire, release)
import qualified Hasql.Session as Hasql (QueryError, run, sql, statement)

--------------------------------------------------------------------------------
-- Connections

-- |
-- Run the given action with a PostgreSQL connection.
-- The connection is closed when the action terminates.
withConnection :: BS.ByteString -> (Connection -> IO a) -> IO a
withConnection settings =
  bracket acquire release
  where
    acquire = Hasql.acquire settings >>= orThrow ConnectionException
    release = Hasql.release

--------------------------------------------------------------------------------
-- Queries

-- |
-- Run a Hasql session and throw if it fails.
runSession :: Connection -> Session a -> IO a
runSession conn session = Hasql.run session conn >>= orThrow QueryException

--------------------------------------------------------------------------------
-- Exceptions

orThrow :: Exception b => (a -> b) -> Either a c -> IO c
orThrow f = either (throwIO . f) pure

-- |
-- Thrown when we cannot connect to PostgreSQL.
newtype ConnectionException =
  ConnectionException Hasql.ConnectionError
  deriving stock (Show)
  deriving anyclass (Exception)

-- |
-- Thrown when a Hasql session fails.
newtype QueryException =
  QueryException Hasql.QueryError
  deriving stock (Show)
  deriving anyclass (Exception)
