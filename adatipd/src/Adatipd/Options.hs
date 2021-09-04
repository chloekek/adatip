-- SPDX-License-Identifier: AGPL-3.0-only

{-# LANGUAGE ApplicativeDo #-}

module Adatipd.Options
  ( Options (..)
  , optionsParserInfo
  , optionsParser
  ) where

import Options.Applicative

import Data.Text (Text)

data Options =
  Options
    { oEnableSignUp :: Bool
    , oInstanceTitle :: Text }
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

  pure Options {..}
