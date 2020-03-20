module Wizard.Api.Resource.Package.PackageDetailSM where

import Data.Swagger

import Shared.Database.Migration.Development.Package.Data.Packages
import Shared.Service.Package.PackageMapper
import Shared.Util.Swagger
import Wizard.Api.Resource.Organization.OrganizationSimpleSM ()
import Wizard.Api.Resource.Package.PackageDetailDTO
import Wizard.Api.Resource.Package.PackageDetailJM ()
import Wizard.Api.Resource.Package.PackageStateSM ()
import Wizard.Database.Migration.Development.Package.Data.Packages
import Wizard.Service.Package.PackageMapper

instance ToSchema PackageDetailDTO where
  declareNamedSchema =
    simpleToSchema
      "_packageDetailDTO"
      (toDetailDTO (toPackage globalPackage) [globalRemotePackage] ["1.0.0"] "https://registry.example.org")
