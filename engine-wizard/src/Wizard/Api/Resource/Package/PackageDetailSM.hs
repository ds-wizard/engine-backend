module Wizard.Api.Resource.Package.PackageDetailSM where

import Data.Swagger

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
    simpleToSchema
      (toDetailDTO
         (toPackage globalPackage)
         [globalRegistryPackage]
         [globalRegistryOrganization]
         ["1.0.0"]
         "https://registry.example.org")
