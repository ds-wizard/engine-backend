module Wizard.Model.Project.Detail.ProjectDetailQuestionnaire where

import qualified Data.UUID as U
import GHC.Generics

import Wizard.Api.Resource.Project.Acl.ProjectPermDTO
import Wizard.Model.Project.File.ProjectFileSimple
import Wizard.Model.Project.Project

data ProjectDetailQuestionnaire = ProjectDetailQuestionnaire
  { uuid :: U.UUID
  , name :: String
  , visibility :: ProjectVisibility
  , sharing :: ProjectSharing
  , knowledgeModelPackageId :: String
  , selectedQuestionTagUuids :: [U.UUID]
  , isTemplate :: Bool
  , migrationUuid :: Maybe U.UUID
  , permissions :: [ProjectPermDTO]
  , files :: [ProjectFileSimple]
  , projectActionsAvailable :: Int
  , projectImportersAvailable :: Int
  }
  deriving (Show, Eq, Generic)
