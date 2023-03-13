module Wizard.Api.Resource.Package.Publish.PackagePublishBranchSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Package.Publish.PackagePublishBranchDTO
import Wizard.Api.Resource.Package.Publish.PackagePublishBranchJM ()
import Wizard.Database.Migration.Development.Branch.Data.Branches

instance ToSchema PackagePublishBranchDTO where
  declareNamedSchema = toSwagger packagePublishBranchDTO
