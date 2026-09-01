module Wizard.Model.Project.Detail.ProjectDetailSettings where

import qualified Data.UUID as U
import GHC.Generics

import Shared.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateDTO
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleDTO
import Wizard.Api.Resource.Project.Acl.ProjectPermDTO
import Wizard.Model.DocumentTemplate.DocumentTemplateState
import Wizard.Model.KnowledgeModel.Locale.KnowledgeModelLocaleList
import Wizard.Model.Project.Project
import Wizard.Model.Project.ProjectState

data ProjectDetailSettings = ProjectDetailSettings
  { uuid :: U.UUID
  , name :: String
  , description :: Maybe String
  , visibility :: ProjectVisibility
  , sharing :: ProjectSharing
  , isTemplate :: Bool
  , permissions :: [ProjectPermDTO]
  , projectTags :: [String]
  , knowledgeModelPackageUuid :: U.UUID
  , knowledgeModelPackage :: KnowledgeModelPackageSimpleDTO
  , knowledgeModelTags :: [Tag]
  , knowledgeModelState :: KnowledgeModelProjectState
  , language :: Maybe String
  , availableLocales :: [KnowledgeModelLocaleList]
  , documentTemplate :: Maybe DocumentTemplateDTO
  , documentTemplateState :: Maybe DocumentTemplateProjectState
  , documentTemplateSupportState :: Maybe DocumentTemplateState
  , documentTemplatePhase :: Maybe DocumentTemplatePhase
  , formatUuid :: Maybe U.UUID
  , selectedQuestionTagUuids :: [U.UUID]
  , fileCount :: Int
  }
  deriving (Show, Eq, Generic)
