module Api.Resource.Migrator.MigratorStateCreateDTO where

data MigratorStateCreateDTO = MigratorStateCreateDTO
  { _migratorStateCreateDTOTargetPackageId :: String
  } deriving (Show, Eq)
