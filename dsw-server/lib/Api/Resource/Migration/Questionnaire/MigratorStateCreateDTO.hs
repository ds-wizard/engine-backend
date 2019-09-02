module Api.Resource.Migration.Questionnaire.MigratorStateCreateDTO where

import qualified Data.UUID as U
import GHC.Generics

data MigratorStateCreateDTO = MigratorStateCreateDTO
  { _migratorStateCreateDTOTargetPackageId :: String
  , _migratorStateCreateDTOTargetTagUuids :: [U.UUID]
  } deriving (Show, Eq, Generic)
