module Wizard.Database.Mapping.Tenant.Tenant where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Wizard.Model.Tenant.Tenant

instance ToRow Tenant where
  toRow Tenant {..} =
    [ toField uuid
    , toField tenantId
    , toField name
    , toField serverDomain
    , toField clientUrl
    , toField enabled
    , toField createdAt
    , toField updatedAt
    , toField serverUrl
    , toField adminServerUrl
    , toField adminClientUrl
    , toField integrationHubServerUrl
    , toField integrationHubClientUrl
    , toField reportingServerUrl
    , toField reportingClientUrl
    , toField signalBridgeUrl
    ]

instance FromRow Tenant where
  fromRow = do
    uuid <- field
    tenantId <- field
    name <- field
    serverDomain <- field
    clientUrl <- field
    enabled <- field
    createdAt <- field
    updatedAt <- field
    serverUrl <- field
    adminServerUrl <- field
    adminClientUrl <- field
    integrationHubServerUrl <- field
    integrationHubClientUrl <- field
    reportingServerUrl <- field
    reportingClientUrl <- field
    signalBridgeUrl <- field
    return $ Tenant {..}
