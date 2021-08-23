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
    { oCardanoNodeSocketPath :: FilePath
    , oEnableSignUp :: Bool
    , oInstanceTitle :: Text }
  deriving stock (Show)

optionsParserInfo :: ParserInfo Options
optionsParserInfo =
  info (optionsParser <**> helper) $
    fullDesc
    <> progDesc "Daemon for serving a single AdaTip instance"

optionsParser :: Parser Options
optionsParser = do

  oCardanoNodeSocketPath <-
    strOption $
      long "cardano-node-socket-path"
      <> metavar "PATH"
      <> help "The same path given to cardano-node \
              \as the --socket-path argument. \
              \Used for talking to cardano-node."

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
