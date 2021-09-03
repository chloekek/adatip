-- SPDX-License-Identifier: AGPL-3.0-only

module Adatipd.SqlTest
  ( withSqlConnection
  , withSqlSettings
  ) where

import System.Directory (createDirectory)
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

      -- Create directory for socket.
      createDirectory (directory <> "/pgsocket")

      -- Create and set up database.
      env <- databaseEnv directory
      withSqlDatabase env $ do
        runScript env "scripts/wait-postgres-ready.bash" []
        runScript env "scripts/setup-database.bash" []
        runScript env "dbmate" ["migrate"]
        runScript env "scripts/seed-database.bash" []

        -- The action should now be able to connect.
        action (postgresSettings directory)

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
  let
    -- Redirect stdout and stderr to /dev/null
    -- so they don’t show up in the test output.
    -- We *could* use Process.std_out and Process.std_err,
    -- but setting those to null is harder than it should be.
    bashScript = "exec \"$@\" > /dev/null 2> /dev/null"
    bashArgs = "-c" : bashScript : "--" : program : args
  in
    (Process.proc "bash" bashArgs)
      { -- Tests are run from adatipd, we need repo root.
        Process.cwd = Just ".."
      , Process.env = Just env }

-- |
-- Obtain environment variables to pass to database-related programs.
-- The environment variables are merged with the existing ones.
databaseEnv :: FilePath -> IO [(String, String)]
databaseEnv directory = do
  oldEnv <- getEnvironment
  pure $

    [ -- Env vars for PostgreSQL tools.
      ("PGDATA", directory <> "/pgdata")
    , ("PGHOST", directory <> "/pgsocket")
    , ("PGPORT", "5432")
    , ("PGUSER", "adatip_setup")
    , ("PGPASSWORD", "adatip_setup")
    , ("PGDATABASE", "adatip")

      -- Env vars for dbmate.
    , ("DATABASE_URL", "postgres:///?socket=" <> directory <> "/pgsocket")
    , ("DBMATE_MIGRATIONS_DIR", "database")
    , ("DBMATE_NO_DUMP_SCHEMA", "true") ]

    -- Original environment.
    <> oldEnv

-- |
-- Compute the settings string to pass to 'Sql.withConnection'.
postgresSettings :: FilePath -> BS.ByteString
postgresSettings directory =
  BS.C8.pack $
    "host=" <> directory <> "/pgsocket " <>
    "port=5432 "           <>
    "user=adatip_app "     <>
    "password=adatip_app " <>
    "dbname=adatip"
