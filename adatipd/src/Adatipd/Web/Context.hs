module Adatipd.Web.Context
  ( Context (..)
  , makeContext

    -- * Convenient re-exports
  , Options (..)
  ) where

import Adatipd.Options (Options (..))

import qualified Adatipd.Sql as Sql (Connection)
import qualified Adatipd.WaiUtil as Wai

-- |
-- Information that is available within any request handler.
-- This information is made available by the routing code in "Adatipd.Web".
--
-- You can just pass this around throughout the request handling code.
-- Even if you do not need all the fields, just passing this around is easier.
data Context =
  Context
    { cOptions :: Options
    , cSqlConn :: Sql.Connection }

-- |
-- Create a context for the current request.
makeContext :: Options -> Sql.Connection -> Wai.Request -> IO Context
makeContext cOptions cSqlConn _request =
  pure Context {..}
