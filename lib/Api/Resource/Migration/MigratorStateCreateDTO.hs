module Api.Resource.Migration.MigratorStateCreateDTO where

data MigratorStateCreateDTO = MigratorStateCreateDTO
  { _migratorStateCreateDTOTargetPackageId :: String
  } deriving (Show, Eq)
