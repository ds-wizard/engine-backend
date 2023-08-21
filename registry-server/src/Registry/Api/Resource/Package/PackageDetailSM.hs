module Registry.Api.Resource.Package.PackageDetailSM where

import Data.Swagger

import Registry.Api.Resource.Package.PackageDetailDTO
import Registry.Api.Resource.Package.PackageDetailJM ()
import Registry.Service.Package.PackageMapper
import RegistryLib.Database.Migration.Development.Organization.Data.Organizations
import Shared.Common.Util.Swagger
import WizardLib.Common.Api.Resource.Organization.OrganizationSimpleSM ()
import WizardLib.KnowledgeModel.Api.Resource.Package.PackagePhaseSM ()
import WizardLib.KnowledgeModel.Database.Migration.Development.Package.Data.Packages
import WizardLib.KnowledgeModel.Service.Package.PackageMapper

instance ToSchema PackageDetailDTO where
  declareNamedSchema = toSwagger (toDetailDTO (toPackage globalPackage) [] orgGlobal)
