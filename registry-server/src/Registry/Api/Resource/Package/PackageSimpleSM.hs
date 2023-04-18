module Registry.Api.Resource.Package.PackageSimpleSM where

import Data.Swagger

import Registry.Api.Resource.Package.PackageSimpleDTO
import Registry.Api.Resource.Package.PackageSimpleJM ()
import Registry.Database.Migration.Development.Organization.Data.Organizations
import Registry.Service.Package.PackageMapper
import Shared.Common.Util.Swagger
import WizardLib.Common.Api.Resource.Organization.OrganizationSimpleSM ()
import WizardLib.KnowledgeModel.Database.Migration.Development.Package.Data.Packages
import WizardLib.KnowledgeModel.Service.Package.PackageMapper

instance ToSchema PackageSimpleDTO where
  declareNamedSchema = toSwagger (toSimpleDTO (toPackage globalPackage) orgGlobal)
