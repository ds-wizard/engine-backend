module Registry.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDetailSM where

import Data.Swagger

import Registry.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDetailDTO
import Registry.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDetailJM ()
import Registry.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper
import RegistryLib.Api.Resource.Organization.OrganizationSimpleSM ()
import RegistryLib.Database.Migration.Development.Organization.Data.Organizations
import Shared.Common.Util.Swagger
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackagePhaseSM ()
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages

instance ToSchema KnowledgeModelPackageDetailDTO where
  declareNamedSchema = toSwagger (toDetailDTO globalKmPackage [] orgGlobal)
