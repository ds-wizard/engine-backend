module Wizard.Api.Resource.Migration.Questionnaire.MigratorStateCreateDTO where

import qualified Data.UUID as U
import GHC.Generics

data MigratorStateCreateDTO = MigratorStateCreateDTO
  { targetPackageId :: String
  , targetTagUuids :: [U.UUID]
  }
  deriving (Show, Eq, Generic)
