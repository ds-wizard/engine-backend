module Shared.ActionKey.Database.Mapping.ActionKey.ActionKey where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Shared.ActionKey.Model.ActionKey.ActionKey

instance (ToField aType, ToField identity) => ToRow (ActionKey identity aType) where
  toRow ActionKey {..} =
    [ toField uuid
    , toField identity
    , toField aType
    , toField hash
    , toField createdAt
    , toField appUuid
    ]

instance (FromField aType, FromField identity) => FromRow (ActionKey identity aType) where
  fromRow = do
    uuid <- field
    identity <- field
    aType <- field
    hash <- field
    createdAt <- field
    appUuid <- field
    return $ ActionKey {..}
