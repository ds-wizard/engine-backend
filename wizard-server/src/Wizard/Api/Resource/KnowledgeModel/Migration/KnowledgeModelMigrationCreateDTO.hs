module Wizard.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationCreateDTO where

import GHC.Generics

data KnowledgeModelMigrationCreateDTO = KnowledgeModelMigrationCreateDTO
  { targetPackageId :: String
  }
  deriving (Show, Eq, Generic)
