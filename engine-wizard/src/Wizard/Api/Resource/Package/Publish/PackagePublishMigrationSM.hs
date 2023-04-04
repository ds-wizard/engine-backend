module Wizard.Api.Resource.Package.Publish.PackagePublishMigrationSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Package.Publish.PackagePublishMigrationDTO
import Wizard.Api.Resource.Package.Publish.PackagePublishMigrationJM ()
import Wizard.Database.Migration.Development.Branch.Data.Branches

instance ToSchema PackagePublishMigrationDTO where
  declareNamedSchema = toSwagger packagePublishMigrationDTO
