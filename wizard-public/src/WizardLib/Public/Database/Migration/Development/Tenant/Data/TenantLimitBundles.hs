module WizardLib.Public.Database.Migration.Development.Tenant.Data.TenantLimitBundles where

import WizardLib.Public.Model.Tenant.Limit.TenantLimitBundleChange

tenantLimitBundleChange :: TenantLimitBundleChange
tenantLimitBundleChange =
  TenantLimitBundleChange
    { users = -2000
    , activeUsers = -1000
    , knowledgeModels = -1000
    , branches = -1000
    , documentTemplates = -1000
    , documentTemplateDrafts = -1000
    , questionnaires = -1000
    , documents = -1000
    , locales = -1000
    , storage = -1000 * 5 * 1000 * 1000
    }
