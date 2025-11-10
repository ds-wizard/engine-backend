module Wizard.Api.Resource.Questionnaire.Migration.MigratorStateCreateDTO where

import qualified Data.UUID as U
import GHC.Generics

data MigratorStateCreateDTO = MigratorStateCreateDTO
  { targetKnowledgeModelPackageId :: String
  , targetTagUuids :: [U.UUID]
  }
  deriving (Show, Eq, Generic)
