-- SPDX-License-Identifier: AGPL-3.0-only

module Adatipd.Sql.Test
  ( withSqlConnection
  , withSqlSettings
  ) where

import Data.Word (Word16)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess, withCreateProcess, waitForProcess)

import qualified Adatipd.Sql as Sql
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Char8 as BS.C8 (pack)
import qualified Data.List as List (intercalate)
import qualified System.Process as Process

--------------------------------------------------------------------------------
-- High-level interface.

-- |
-- Set up a temporary database and connect to it.
-- When the given action returns, the database is destroyed.
withSqlConnection :: (Sql.Connection -> IO a) -> IO a
withSqlConnection = withSqlSettings . flip Sql.withConnection

-- |
-- Set up a temporary database.
-- The given action receives the connection settings.
-- When the given action returns, the database is destroyed.
withSqlSettings :: (BS.ByteString -> IO a) -> IO a
withSqlSettings action =

  -- Create temporary directory to store database in.
  withSystemTempDirectory "adatipd-test-database" $
    \directory -> do

      -- TODO: When #42 is fixed we do not need to set a port number.
      let port = 6000

      -- Create and set up database.
      env <- databaseEnv directory port
      withSqlDatabase env $ do
        runScript env "scripts/wait-postgres-ready.bash" []
        runScript env "scripts/setup-database.bash" []
        runScript env "dbmate" ["migrate"]
        runScript env "scripts/seed-database.bash" []

        -- The action should now be able to connect.
        action (postgresSettings port)

--------------------------------------------------------------------------------
-- Database commands.

-- |
-- Set up a temporary database.
-- When the given action returns, the database is destroyed.
withSqlDatabase :: [(String, String)] -> IO a -> IO a
withSqlDatabase env action =
  withCreateProcess (mkCreateProcess env "scripts/run-postgres.bash" []) $
    \_stdin _stdout _stderr _pid ->
      action

-- |
-- Run a script with the environment variables.
-- Throw if the script does not return successfully.
runScript :: [(String, String)] -> FilePath -> [String] -> IO ()
runScript env script args =
  withCreateProcess (mkCreateProcess env script args) $
    \_stdin _stdout _stderr pid ->
      waitForProcess pid >>= \case
        ExitSuccess -> pure ()
        ExitFailure {} -> fail (List.intercalate " " (script : args))

--------------------------------------------------------------------------------
-- PostgreSQL environment.

mkCreateProcess :: [(String, String)] -> FilePath -> [String] -> CreateProcess
mkCreateProcess env program args =
  (Process.proc program args)
    { Process.cwd = Just ".." -- Tests are run from adatipd, we need repo root.
    , Process.env = Just env }

-- |
-- Obtain environment variables to pass to database-related programs.
-- The environment variables are merged with the existing ones.
databaseEnv :: FilePath -> Word16 -> IO [(String, String)]
databaseEnv directory port = do
  oldEnv <- getEnvironment
  pure $

    [ -- Env vars for PostgreSQL tools.
      ("PGDATA", directory <> "/pgdata")
    , ("PGHOST", "127.0.0.1")
    , ("PGPORT", show port)
    , ("PGUSER", "adatip_setup")
    , ("PGPASSWORD", "adatip_setup")
    , ("PGDATABASE", "adatip")

      -- Env vars for dbmate.
    , ("DATABASE_URL", "postgres://127.0.0.1:" <> show port <> "/?sslmode=disable")
    , ("DBMATE_MIGRATIONS_DIR", "database")
    , ("DBMATE_NO_DUMP_SCHEMA", "true") ]

    -- Original environment.
    <> oldEnv

-- |
-- Compute the settings string to pass to 'Sql.withConnection'.
postgresSettings :: Word16 -> BS.ByteString
postgresSettings port =
  BS.C8.pack $
    "host=127.0.0.1 "           <>
    "port=" <> show port <> " " <>
    "user=adatip_app "          <>
    "password=adatip_app "      <>
    "dbname=adatip"
