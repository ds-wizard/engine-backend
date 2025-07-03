module Wizard.Database.Mapping.Tenant.Config.TenantConfigDashboardAndLoginScreen where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Shared.Common.Database.Mapping.Common
import Wizard.Model.Tenant.Config.TenantConfig
import WizardLib.Public.Database.Mapping.Tenant.Config.TenantConfigDashboardAndLoginScreen ()

instance ToRow TenantConfigDashboardAndLoginScreen where
  toRow TenantConfigDashboardAndLoginScreen {..} =
    [ toField tenantUuid
    , toField dashboardType
    , toField loginInfo
    , toField loginInfoSidebar
    , toField createdAt
    , toField updatedAt
    ]

instance FromRow TenantConfigDashboardAndLoginScreen where
  fromRow = do
    tenantUuid <- field
    dashboardType <- field
    let announcements = []
    loginInfo <- field
    loginInfoSidebar <- field
    createdAt <- field
    updatedAt <- field
    return $ TenantConfigDashboardAndLoginScreen {..}

instance ToField TenantConfigDashboardAndLoginScreenDashboardType where
  toField = toFieldGenericEnum

instance FromField TenantConfigDashboardAndLoginScreenDashboardType where
  fromField = fromFieldGenericEnum
