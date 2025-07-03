module WizardLib.Public.Database.Mapping.Tenant.Config.TenantConfigLookAndFeel where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import WizardLib.Public.Model.Tenant.Config.TenantConfig

instance ToRow TenantConfigLookAndFeel where
  toRow TenantConfigLookAndFeel {..} =
    [ toField tenantUuid
    , toField appTitle
    , toField appTitleShort
    , toField logoUrl
    , toField primaryColor
    , toField illustrationsColor
    , toField createdAt
    , toField updatedAt
    ]

instance FromRow TenantConfigLookAndFeel where
  fromRow = do
    tenantUuid <- field
    appTitle <- field
    appTitleShort <- field
    let customMenuLinks = []
    logoUrl <- field
    primaryColor <- field
    illustrationsColor <- field
    createdAt <- field
    updatedAt <- field
    return $ TenantConfigLookAndFeel {..}

instance FromRow TenantConfigLookAndFeelCustomMenuLink

instance ToRow TenantConfigLookAndFeelCustomMenuLink
