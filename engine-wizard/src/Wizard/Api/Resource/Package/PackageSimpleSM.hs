module Wizard.Api.Resource.Package.PackageSimpleSM where

import Data.Swagger

import Shared.Api.Resource.Package.PackagePhaseSM ()
import Shared.Database.Migration.Development.Package.Data.Packages
import Shared.Model.Package.PackageSimple
import Shared.Service.Package.PackageMapper
import Shared.Util.Swagger
import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Api.Resource.Package.PackageSimpleJM ()
import Wizard.Api.Resource.Package.PackageStateSM ()
import Wizard.Api.Resource.Registry.RegistryOrganizationSM ()
import Wizard.Service.Package.PackageMapper

instance ToSchema PackageSimpleDTO where
  declareNamedSchema = toSwagger (toSimpleDTO (toPackage globalPackage))

instance ToSchema PackageSimple where
  declareNamedSchema = toSwagger (toSimple . toPackage $ globalPackage)
