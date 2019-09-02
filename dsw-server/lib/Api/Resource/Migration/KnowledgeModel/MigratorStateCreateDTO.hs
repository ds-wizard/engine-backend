module Api.Resource.Migration.KnowledgeModel.MigratorStateCreateDTO where

import GHC.Generics

data MigratorStateCreateDTO = MigratorStateCreateDTO
  { _migratorStateCreateDTOTargetPackageId :: String
  } deriving (Show, Eq, Generic)
