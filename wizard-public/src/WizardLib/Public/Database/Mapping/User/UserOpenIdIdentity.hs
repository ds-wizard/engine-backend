module WizardLib.Public.Database.Mapping.User.UserOpenIdIdentity where

import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import WizardLib.Public.Model.User.UserOpenIdIdentity

instance ToRow UserOpenIdIdentity where
  toRow UserOpenIdIdentity {..} =
    [ toField uuid
    , toField externalId
    , toField externalLabel
    , toField userUuid
    , toField providerUuid
    , toField tenantUuid
    , toField createdAt
    ]

instance FromRow UserOpenIdIdentity where
  fromRow = do
    uuid <- field
    externalId <- field
    externalLabel <- field
    userUuid <- field
    providerUuid <- field
    tenantUuid <- field
    createdAt <- field
    return $ UserOpenIdIdentity {..}
