module Wizard.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationCreateDTO where

import qualified Data.UUID as U
import GHC.Generics

data KnowledgeModelMigrationCreateDTO = KnowledgeModelMigrationCreateDTO
  { targetPackageUuid :: U.UUID
  }
  deriving (Show, Eq, Generic)
