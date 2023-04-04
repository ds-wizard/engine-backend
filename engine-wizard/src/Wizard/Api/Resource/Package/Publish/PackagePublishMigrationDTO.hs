module Wizard.Api.Resource.Package.Publish.PackagePublishMigrationDTO where

import qualified Data.UUID as U
import GHC.Generics

data PackagePublishMigrationDTO = PackagePublishMigrationDTO
  { branchUuid :: U.UUID
  , version :: String
  , description :: String
  , readme :: String
  }
  deriving (Generic)
