{-# LANGUAGE OverloadedStrings #-}

-- |
-- This module aids in writing end-to-end tests.
-- Within the 'E2E' monad, you can send requests to the request handler.
-- The response will be returned, which you can then inspect.
module Adatipd.E2ETest
  ( E2E
  , runE2E
  , runE2E'
  , testOptions
  , sendRequest
  , sendRequest'
  ) where

import Adatipd.Options (Options (..))
import Adatipd.SqlTest (withSqlConnection)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Network.HTTP.Types.Method (Method)

import qualified Adatipd.Sql as Sql (Connection)
import qualified Adatipd.WaiUtil as Wai
import qualified Adatipd.Web as Web (handle)
import qualified Control.Monad.Trans.Reader as Reader (asks)
import qualified Network.Wai.Internal as Wai (ResponseReceived (..))

newtype E2E a =
  E2E (ReaderT Env IO a)
  deriving newtype (Functor, Applicative, Monad)

data Env =
  Env
    { eOptions :: Options
    , eSqlConn :: Sql.Connection }

-- |
-- Run an end-to-end test within a configurable environment.
-- This function will use 'testOptions' for the options,
-- but those can be adjusted by passing a custom function.
-- This function will spin up a test database for use by the test,
-- so you do not need to do that yourself within the test.
runE2E :: (Options -> Options) -> E2E a -> IO a
runE2E changeOptions action =
  withSqlConnection $
    \sqlConn ->
      let options = changeOptions testOptions in
      runE2E' options sqlConn action

-- |
-- Lower-level version of 'runE2E' that gives the caller
-- more control over the environment.
runE2E' :: Options -> Sql.Connection -> E2E a -> IO a
runE2E' eOptions eSqlConn (E2E action) =
  runReaderT action Env {..}

-- |
-- Reasonable options for end-to-end tests.
testOptions :: Options
testOptions =
  let
    oEnableSignUp = True
    oInstanceTitle = "adatip.test"
  in
    Options {..}

-- |
-- Send a request to the request handler and return the response.
sendRequest :: Method -> [Text] -> E2E Wai.Response
sendRequest requestMethod pathInfo =
  sendRequest' $
    Wai.defaultRequest
      { Wai.requestMethod = requestMethod
      , Wai.pathInfo = pathInfo }

-- |
-- Lower-level version of 'sendRequest'.
sendRequest' :: Wai.Request -> E2E Wai.Response
sendRequest' request = E2E $ do
  options <- Reader.asks eOptions
  sqlConn <- Reader.asks eSqlConn
  liftIO $ do
    response <- newIORef Nothing
    let writeResponse response' = do
          writeIORef response (Just response')
          pure Wai.ResponseReceived
    Wai.ResponseReceived <-
      Web.handle options sqlConn request writeResponse
    readIORef response >>= \case
      Nothing -> fail "Request handler neglected writing a response"
      Just response' -> pure response'
