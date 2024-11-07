module Wizard.Database.Mapping.Tenant.Tenant where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Shared.Common.Database.Mapping.Common
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
    , toField analyticsServerUrl
    , toField analyticsClientUrl
    , toField signalBridgeUrl
    , toField state
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
    analyticsServerUrl <- field
    analyticsClientUrl <- field
    signalBridgeUrl <- field
    state <- field
    return $ Tenant {..}

instance ToField TenantState where
  toField = toFieldGenericEnum

instance FromField TenantState where
  fromField = fromFieldGenericEnum
