module Wizard.Api.Resource.Tenant.Config.TenantConfigSubmissionServiceSimpleSM where

import Data.Swagger

import Shared.Common.Api.Resource.Config.SimpleFeatureSM ()
import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Tenant.Config.TenantConfigSubmissionServiceSimpleJM ()
import Wizard.Database.Migration.Development.Tenant.Data.TenantConfigs
import Wizard.Model.Tenant.Config.TenantConfigSubmissionServiceSimple
import Wizard.Service.Tenant.Config.ConfigMapper

instance ToSchema TenantConfigSubmissionServiceSimple where
  declareNamedSchema = toSwagger (toSubmissionServiceSimple defaultSubmissionService)
