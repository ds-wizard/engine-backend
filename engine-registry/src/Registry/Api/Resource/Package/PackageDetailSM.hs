module Registry.Api.Resource.Package.PackageDetailSM where

import Data.Swagger

import Registry.Api.Resource.Package.PackageDetailDTO
import Registry.Api.Resource.Package.PackageDetailJM ()
import Registry.Database.Migration.Development.Organization.Data.Organizations
import Registry.Service.Package.PackageMapper
import Shared.Api.Resource.Organization.OrganizationSimpleSM ()
import Shared.Database.Migration.Development.Package.Data.Packages
import Shared.Service.Package.PackageMapper
import Shared.Util.Swagger

instance ToSchema PackageDetailDTO where
  declareNamedSchema = simpleToSchema "_packageDetailDTO" (toDetailDTO (toPackage globalPackage) [] orgGlobal)
