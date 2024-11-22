module WizardLib.Public.Api.Resource.Tenant.Limit.TenantLimitBundleChangeSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import WizardLib.Public.Database.Migration.Development.Tenant.Data.TenantLimitBundles
import WizardLib.Public.Model.Tenant.Limit.TenantLimitBundleChange

instance ToSchema TenantLimitBundleChange where
  declareNamedSchema = toSwagger tenantLimitBundleChange
