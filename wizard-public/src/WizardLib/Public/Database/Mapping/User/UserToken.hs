module WizardLib.Public.Database.Mapping.User.UserToken where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Shared.Common.Database.Mapping.Common
import WizardLib.Public.Model.User.UserToken

instance ToField UserTokenType where
  toField = toFieldGenericEnum

instance FromField UserTokenType where
  fromField = fromFieldGenericEnum

instance ToRow UserToken where
  toRow UserToken {..} =
    [ toField uuid
    , toField userUuid
    , toField value
    , toField sessionState
    , toField tenantUuid
    , toField createdAt
    , toField name
    , toField tType
    , toField userAgent
    , toField expiresAt
    ]

instance FromRow UserToken where
  fromRow = do
    uuid <- field
    userUuid <- field
    value <- field
    sessionState <- field
    tenantUuid <- field
    createdAt <- field
    name <- field
    tType <- field
    userAgent <- field
    expiresAt <- field
    return $ UserToken {..}
