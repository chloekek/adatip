-- SPDX-License-Identifier: AGPL-3.0-only

{-# LANGUAGE OverloadedStrings #-}

module Adatipd.Nickname
  ( Nickname
  , format
  , parse
  , formatUriComponent
  , parseUriComponent
  ) where

import Data.Text (Text)

import qualified Data.Text as Text (any, length, null, toLower, uncons)

-- |
-- Nickname uniquely identifying a creator.
newtype Nickname =
  Nickname Text
  deriving stock (Eq, Show)

-- |
-- Retrieve the text form of a nickname.
format :: Nickname -> Text
format (Nickname text) = text

-- |
-- Validate a nickname and convert it to lowercase.
-- The nickname must be non-empty and contain at most 20 characters.
-- The following characters are allowed: @a-z A-Z 0-9@.
-- The nickname must be in the correct format,
-- otherwise a descriptive error message is returned.
parse :: Text -> Either String Nickname
parse text
  | Text.null text =
      Left "Nickname is empty"
  | Text.length text > 20 =
      Left "Nickname is too long"
  | Text.any (not . isValidChar) text =
      Left "Nickname contains invalid characters"
  | otherwise =
      Right (Nickname (Text.toLower text))

-- |
-- Whether a character may occur in a nickname.
isValidChar :: Char -> Bool
isValidChar c =
  (c >= 'a' && c <= 'z') ||
  (c >= 'A' && c <= 'Z') ||
  (c >= '0' && c <= '9')

-- |
-- Format a nickname to be the URI of the creator’s home page.
-- This just prefixes a @~@ to the nickname.
formatUriComponent :: Nickname -> Text
formatUriComponent = ("~" <>) . format

-- |
-- The mandatory @~@ prefix is removed.
-- The nickname is then parsed using 'parse'.
parseUriComponent :: Text -> Either String Nickname
parseUriComponent text =
  case Text.uncons text of
    Just ('~', nickname) -> parse nickname
    Just _ -> Left "Nickname missing ‘~’ prefix"
    _ -> Left "Nickname is empty"
