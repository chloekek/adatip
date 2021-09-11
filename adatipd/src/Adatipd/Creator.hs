-- SPDX-License-Identifier: AGPL-3.0-only

{-# LANGUAGE OverloadedStrings #-}

module Adatipd.Creator
  ( CreatorId (..)
  , CreatorInfo (..)
  , fetchCreatorIdAndCurrentNickname
  , encodeCreatorId
  , fetchCreatorInfo
  ) where

import Adatipd.Nickname (Nickname)
import Data.Functor.Contravariant ((>$<))
import Data.Text (Text)
import PostgreSQL.Binary.Data (UUID)

import qualified Adatipd.Nickname as Nickname
import qualified Adatipd.Sql as Sql
import qualified Hasql.Decoders as SqlDec
import qualified Hasql.Encoders as SqlEnc

newtype CreatorId = CreatorId { getCreatorId :: UUID }
  deriving newtype (Show)

encodeCreatorId :: SqlEnc.Params CreatorId
encodeCreatorId =
  SqlEnc.param (SqlEnc.nonNullable (getCreatorId >$< SqlEnc.uuid))

decodeNickname :: CreatorId -> SqlDec.Row Nickname
decodeNickname (CreatorId creatorId) = do
  nicknameRaw <- SqlDec.column (SqlDec.nonNullable SqlDec.text)
  case Nickname.parse nicknameRaw of
    Right nickname -> pure nickname
    Left err -> fail $
      "Invalid nickname for creator " <> show creatorId <> ": " <> err

-- | Given any nickname for the creator, return their id and current nickname.
fetchCreatorIdAndCurrentNickname
  :: Sql.Connection
  -> Nickname
  -> IO (Maybe (CreatorId, Nickname))
fetchCreatorIdAndCurrentNickname sqlConn nickname =
  let
    encodeNickname :: SqlEnc.Params Nickname
    encodeNickname =
      SqlEnc.param (SqlEnc.nonNullable (Nickname.format >$< SqlEnc.text))

    decode :: SqlDec.Row (CreatorId, Nickname)
    decode = do
      creatorId       <- CreatorId <$> SqlDec.column (SqlDec.nonNullable SqlDec.uuid)
      currentNickname <- decodeNickname creatorId
      pure (creatorId, currentNickname)

  in
    Sql.runSession sqlConn $ Sql.statement nickname $
      Sql.Statement
        "SELECT\n\
        \  creator_id, \n\
        \  creator_current_nickname(creator_id)\n\
        \FROM\n\
        \  creator_nicknames\n\
        \WHERE\n\
        \  nickname = $1"
        encodeNickname
        (SqlDec.rowMaybe decode)
        False

data CreatorInfo =
  CreatorInfo
    { ciNickname :: Nickname
    , ciName :: Text
    , ciBiography :: Text
    }

fetchCreatorInfo :: Sql.Connection -> CreatorId -> IO CreatorInfo
fetchCreatorInfo sqlConn creatorId =

  Sql.runSession sqlConn $
    Sql.statement creatorId $
      Sql.Statement
        "SELECT\n\
        \  creator_current_nickname($1),\n\
        \  creator_current_name($1),\n\
        \  creator_current_biography($1)"
        encodeCreatorId
        (SqlDec.singleRow decodeCreatorInfo)
        False

  where

    decodeCreatorInfo :: SqlDec.Row CreatorInfo
    decodeCreatorInfo = do
      ciNickname  <- decodeNickname creatorId
      ciName      <- SqlDec.column (SqlDec.nonNullable SqlDec.text)
      ciBiography <- SqlDec.column (SqlDec.nonNullable SqlDec.text)
      pure CreatorInfo {..}
