module Wizard.Api.Resource.Package.PackageChangeSM where

import Data.Swagger

import Shared.Api.Resource.Package.PackagePhaseSM ()
import Shared.Database.Migration.Development.Package.Data.Packages
import Shared.Service.Package.PackageMapper
import Shared.Util.Swagger
import Wizard.Api.Resource.Package.PackageChangeDTO
import Wizard.Api.Resource.Package.PackageChangeJM ()
import Wizard.Service.Package.PackageMapper

instance ToSchema PackageChangeDTO where
  declareNamedSchema = toSwagger (toChangeDTO (toPackage globalPackage))
