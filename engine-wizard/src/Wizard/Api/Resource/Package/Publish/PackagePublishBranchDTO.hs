module Wizard.Api.Resource.Package.Publish.PackagePublishBranchDTO where

import qualified Data.UUID as U
import GHC.Generics

data PackagePublishBranchDTO = PackagePublishBranchDTO
  { branchUuid :: U.UUID
  }
  deriving (Generic)
