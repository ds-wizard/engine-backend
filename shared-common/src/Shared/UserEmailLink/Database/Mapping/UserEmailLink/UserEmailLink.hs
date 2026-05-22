module Shared.UserEmailLink.Database.Mapping.UserEmailLink.UserEmailLink where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Shared.UserEmailLink.Model.UserEmailLink.UserEmailLink

instance (ToField aType, ToField identity) => ToRow (UserEmailLink identity aType) where
  toRow UserEmailLink {..} =
    [ toField uuid
    , toField identity
    , toField aType
    , toField hash
    , toField createdAt
    , toField tenantUuid
    ]

instance (FromField aType, FromField identity) => FromRow (UserEmailLink identity aType) where
  fromRow = do
    uuid <- field
    identity <- field
    aType <- field
    hash <- field
    createdAt <- field
    tenantUuid <- field
    return $ UserEmailLink {..}
