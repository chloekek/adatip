-- SPDX-License-Identifier: AGPL-3.0-only

{-# LANGUAGE ApplicativeDo #-}

module Adatipd.Options
  ( Options (..)
  , optionsParserInfo
  , optionsParser
  ) where

import Options.Applicative

import Data.Text (Text)
import Data.Time.Clock (DiffTime, secondsToDiffTime)

data Options =
  Options
    { oEnableSignUp :: Bool
    , oInstanceTitle :: Text
    , oSessionMaxAge :: DiffTime }
  deriving stock (Show)

optionsParserInfo :: ParserInfo Options
optionsParserInfo =
  info (optionsParser <**> helper) $
    fullDesc
    <> progDesc "Daemon for serving a single Adatip instance"

optionsParser :: Parser Options
optionsParser = do

  oEnableSignUp <-
    switch $
      long "enable-sign-up"
      <> help "If given, any visitor may sign up to become a creator. \
              \If not given, new creators can be added \
              \only by an instance administrator."

  oInstanceTitle <-
    strOption $
      long "instance-title"
      <> metavar "TITLE"
      <> help "The title of the instance, as free-form text. \
              \Shown to visitors in various parts of the user interface."

  oSessionMaxAge <-
    option (secondsToDiffTime <$> auto) $
      long "session-max-age-seconds"
      <> metavar "SECONDS"
      <> value (60 * 60 * 24 * 365)
      <> help "The maximum age of the sessionId cookie. \
              \After this time of not visiting the website, \
              \the sessionId cookie will be discarded by the browser, \
              \causing the user to lose the session. \
              \Larger values are more insecure."

  pure Options {..}
