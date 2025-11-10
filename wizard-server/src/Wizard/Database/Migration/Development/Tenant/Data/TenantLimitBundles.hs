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
    , users = -1000
    , activeUsers = -1000
    , knowledgeModels = -1000
    , knowledgeModelEditors = -1000
    , documentTemplates = -1000
    , documentTemplateDrafts = -1000
    , questionnaires = -1000
    , documents = -1000
    , locales = -1000
    , storage = -1000 * 5 * 1000 * 1000
    , createdAt = dt' 2018 1 25
    , updatedAt = dt' 2018 1 25
    }

defaultTenantLimitBundleEdited :: TenantLimitBundle
defaultTenantLimitBundleEdited =
  defaultTenantLimitBundle
    { users = -2000
    }

differentTenantLimitBundle :: TenantLimitBundle
differentTenantLimitBundle =
  TenantLimitBundle
    { uuid = differentTenant.uuid
    , users = -1000
    , activeUsers = -1000
    , knowledgeModels = -1000
    , knowledgeModelEditors = -1000
    , documentTemplates = -1000
    , documentTemplateDrafts = -1000
    , questionnaires = -1000
    , documents = -1000
    , locales = -1000
    , storage = -1000 * 5 * 1000 * 1000
    , createdAt = dt' 2018 1 25
    , updatedAt = dt' 2018 1 25
    }
