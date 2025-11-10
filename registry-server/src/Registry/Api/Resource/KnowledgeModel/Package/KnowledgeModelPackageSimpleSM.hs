module Registry.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleSM where

import Data.Swagger

import Registry.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper
import RegistryLib.Api.Resource.Organization.OrganizationSimpleSM ()
import RegistryLib.Api.Resource.Package.KnowledgeModelPackageSimpleDTO
import RegistryLib.Api.Resource.Package.KnowledgeModelPackageSimpleJM ()
import RegistryLib.Database.Migration.Development.Organization.Data.Organizations
import Shared.Common.Util.Swagger
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages

instance ToSchema KnowledgeModelPackageSimpleDTO where
  declareNamedSchema = toSwagger (toSimpleDTO globalKmPackage orgGlobal)
