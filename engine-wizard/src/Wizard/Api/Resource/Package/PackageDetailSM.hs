module Wizard.Api.Resource.Package.PackageDetailSM where

import Data.Swagger

import Shared.Api.Resource.Package.PackagePhaseSM ()
import Shared.Database.Migration.Development.Package.Data.Packages
import Shared.Service.Package.PackageMapper
import Shared.Util.Swagger
import Wizard.Api.Resource.Package.PackageDetailDTO
import Wizard.Api.Resource.Package.PackageDetailJM ()
import Wizard.Api.Resource.Package.PackageStateSM ()
import Wizard.Api.Resource.Registry.RegistryOrganizationSM ()
import Wizard.Database.Migration.Development.Registry.Data.RegistryOrganizations
import Wizard.Database.Migration.Development.Registry.Data.RegistryPackages
import Wizard.Service.Package.PackageMapper

instance ToSchema PackageDetailDTO where
  declareNamedSchema =
    toSwagger
      ( toDetailDTO
          (toPackage globalPackage)
          [globalRegistryPackage]
          [globalRegistryOrganization]
          ["1.0.0"]
          (Just "https://registry.example.org")
      )
