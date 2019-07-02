module Api.Resource.Migration.KnowledgeModel.MigratorStateCreateDTO where

data MigratorStateCreateDTO = MigratorStateCreateDTO
  { _migratorStateCreateDTOTargetPackageId :: String
  } deriving (Show, Eq)
