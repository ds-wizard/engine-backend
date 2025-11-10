module Wizard.Api.Resource.KnowledgeModel.Package.Publish.KnowledgeModelPackagePublishMigrationDTO where

import qualified Data.UUID as U
import GHC.Generics

data PackagePublishMigrationDTO = PackagePublishMigrationDTO
  { editorUuid :: U.UUID
  , version :: String
  , description :: String
  , readme :: String
  }
  deriving (Generic)
