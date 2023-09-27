module Wizard.Database.Migration.Development.Tenant.Data.TenantLimitBundles where

import Shared.Common.Constant.Tenant
import Shared.Common.Util.Date
import Wizard.Database.Migration.Development.Tenant.Data.Tenants
import Wizard.Model.Tenant.Limit.TenantLimitBundle
import Wizard.Model.Tenant.Tenant

defaultTenantLimitBundle :: TenantLimitBundle
defaultTenantLimitBundle =
  TenantLimitBundle
    { uuid = defaultTenantUuid
    , users = Nothing
    , activeUsers = Nothing
    , knowledgeModels = Nothing
    , branches = Nothing
    , documentTemplates = Nothing
    , documentTemplateDrafts = Nothing
    , questionnaires = Nothing
    , documents = Nothing
    , locales = Nothing
    , storage = Nothing
    , createdAt = dt' 2018 1 25
    , updatedAt = dt' 2018 1 25
    }

differentTenantLimitBundle :: TenantLimitBundle
differentTenantLimitBundle =
  TenantLimitBundle
    { uuid = differentTenant.uuid
    , users = Nothing
    , activeUsers = Nothing
    , knowledgeModels = Nothing
    , branches = Nothing
    , documentTemplates = Nothing
    , documentTemplateDrafts = Nothing
    , questionnaires = Nothing
    , documents = Nothing
    , locales = Nothing
    , storage = Nothing
    , createdAt = dt' 2018 1 25
    , updatedAt = dt' 2018 1 25
    }
