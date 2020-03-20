module Registry.Api.Resource.Package.PackageSimpleSM where

import Data.Swagger

import Registry.Api.Resource.Organization.OrganizationSimpleSM ()
import Registry.Api.Resource.Package.PackageSimpleDTO
import Registry.Api.Resource.Package.PackageSimpleJM ()
import Registry.Database.Migration.Development.Organization.Data.Organizations
import Registry.Service.Package.PackageMapper
import Shared.Database.Migration.Development.Package.Data.Packages
import Shared.Service.Package.PackageMapper
import Shared.Util.Swagger

instance ToSchema PackageSimpleDTO where
  declareNamedSchema = simpleToSchema "_packageSimpleDTO" (toSimpleDTO (toPackage globalPackage) orgGlobal)
