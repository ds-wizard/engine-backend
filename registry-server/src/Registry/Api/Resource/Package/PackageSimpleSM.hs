module Registry.Api.Resource.Package.PackageSimpleSM where

import Data.Swagger

import Registry.Service.Package.PackageMapper
import RegistryLib.Api.Resource.Organization.OrganizationSimpleSM ()
import RegistryLib.Api.Resource.Package.PackageSimpleDTO
import RegistryLib.Api.Resource.Package.PackageSimpleJM ()
import RegistryLib.Database.Migration.Development.Organization.Data.Organizations
import Shared.Common.Util.Swagger
import WizardLib.KnowledgeModel.Database.Migration.Development.Package.Data.Packages
import WizardLib.KnowledgeModel.Service.Package.PackageMapper

instance ToSchema PackageSimpleDTO where
  declareNamedSchema = toSwagger (toSimpleDTO (toPackage globalPackage) orgGlobal)
