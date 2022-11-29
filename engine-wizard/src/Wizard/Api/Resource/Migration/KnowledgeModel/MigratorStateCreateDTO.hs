module Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateCreateDTO where

import GHC.Generics

data MigratorStateCreateDTO = MigratorStateCreateDTO
  { targetPackageId :: String
  }
  deriving (Show, Eq, Generic)
