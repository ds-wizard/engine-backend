module Wizard.Api.Resource.Project.Migration.ProjectMigrationDTO where

import qualified Data.UUID as U
import GHC.Generics

import Wizard.Api.Resource.Project.Detail.ProjectDetailQuestionnaireDTO

data ProjectMigrationDTO = ProjectMigrationDTO
  { oldProject :: ProjectDetailQuestionnaireDTO
  , newProject :: ProjectDetailQuestionnaireDTO
  , resolvedQuestionUuids :: [U.UUID]
  , tenantUuid :: U.UUID
  }
  deriving (Show, Eq, Generic)
