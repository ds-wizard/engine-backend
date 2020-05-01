module Wizard.Api.Resource.Package.PackageSimpleSM where

import Data.Swagger

import Shared.Api.Resource.Organization.OrganizationSimpleSM ()
import Shared.Database.Migration.Development.Package.Data.Packages
import Shared.Service.Package.PackageMapper
import Shared.Util.Swagger
import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Api.Resource.Package.PackageSimpleJM ()
import Wizard.Api.Resource.Package.PackageStateSM ()
import Wizard.Service.Package.PackageMapper

instance ToSchema PackageSimpleDTO where
  declareNamedSchema = simpleToSchema (toSimpleDTO (toPackage globalPackage))
