module Shared.Api.Resource.Package.PackageSM where

import Data.Swagger

import Shared.Api.Resource.Event.EventSM ()
import Shared.Api.Resource.Package.PackageDTO
import Shared.Api.Resource.Package.PackageJM ()
import Shared.Api.Resource.Package.PackagePhaseSM ()
import Shared.Database.Migration.Development.Package.Data.Packages
import Shared.Util.Swagger

instance ToSchema PackageDTO where
  declareNamedSchema = toSwagger globalPackageDto
