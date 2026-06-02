module WizardLib.Public.Database.Mapping.User.UserRegistrationPending where

import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import WizardLib.Public.Model.User.UserRegistrationPending

instance ToField serviceType => ToRow (UserRegistrationPending serviceType) where
  toRow UserRegistrationPending {..} =
    [ toField uuid
    , toField hash
    , toField serviceType
    , toField providerUuid
    , toField externalId
    , toField externalLabel
    , toField email
    , toField firstName
    , toField lastName
    , toField imageUrl
    , toField affiliation
    , toField tenantUuid
    , toField createdAt
    ]

instance FromField serviceType => FromRow (UserRegistrationPending serviceType) where
  fromRow = do
    uuid <- field
    hash <- field
    serviceType <- field
    providerUuid <- field
    externalId <- field
    externalLabel <- field
    email <- field
    firstName <- field
    lastName <- field
    imageUrl <- field
    affiliation <- field
    tenantUuid <- field
    createdAt <- field
    return $ UserRegistrationPending {..}
