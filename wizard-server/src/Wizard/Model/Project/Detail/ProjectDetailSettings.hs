module Wizard.Model.Project.Detail.ProjectDetailSettings where

import qualified Data.UUID as U
import GHC.Generics

import Shared.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateDTO
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleDTO
import Wizard.Api.Resource.Project.Acl.ProjectPermDTO
import Wizard.Model.DocumentTemplate.DocumentTemplateState
import Wizard.Model.Project.Project

data ProjectDetailSettings = ProjectDetailSettings
  { uuid :: U.UUID
  , name :: String
  , description :: Maybe String
  , visibility :: ProjectVisibility
  , sharing :: ProjectSharing
  , isTemplate :: Bool
  , migrationUuid :: Maybe U.UUID
  , permissions :: [ProjectPermDTO]
  , projectTags :: [String]
  , knowledgeModelPackageId :: String
  , knowledgeModelPackage :: KnowledgeModelPackageSimpleDTO
  , knowledgeModelTags :: [Tag]
  , documentTemplate :: Maybe DocumentTemplateDTO
  , documentTemplateState :: Maybe DocumentTemplateState
  , documentTemplatePhase :: Maybe DocumentTemplatePhase
  , formatUuid :: Maybe U.UUID
  , selectedQuestionTagUuids :: [U.UUID]
  , fileCount :: Int
  }
  deriving (Show, Eq, Generic)
