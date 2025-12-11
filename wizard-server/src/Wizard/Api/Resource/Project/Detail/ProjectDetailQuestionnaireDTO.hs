module Wizard.Api.Resource.Project.Detail.ProjectDetailQuestionnaireDTO where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import GHC.Generics

import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Wizard.Api.Resource.Project.Acl.ProjectPermDTO
import Wizard.Model.Project.File.ProjectFileSimple
import Wizard.Model.Project.Project
import Wizard.Model.Project.ProjectReply

data ProjectDetailQuestionnaireDTO = ProjectDetailQuestionnaireDTO
  { uuid :: U.UUID
  , name :: String
  , visibility :: ProjectVisibility
  , sharing :: ProjectSharing
  , knowledgeModelPackageId :: String
  , selectedQuestionTagUuids :: [U.UUID]
  , isTemplate :: Bool
  , knowledgeModel :: KnowledgeModel
  , replies :: M.Map String Reply
  , labels :: M.Map String [U.UUID]
  , phaseUuid :: Maybe U.UUID
  , migrationUuid :: Maybe U.UUID
  , permissions :: [ProjectPermDTO]
  , files :: [ProjectFileSimple]
  , unresolvedCommentCounts :: M.Map String (M.Map U.UUID Int)
  , resolvedCommentCounts :: M.Map String (M.Map U.UUID Int)
  , projectActionsAvailable :: Int
  , projectImportersAvailable :: Int
  , fileCount :: Int
  }
  deriving (Show, Eq, Generic)
