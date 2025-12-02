module Wizard.Model.Questionnaire.QuestionnaireDetailSettings where

import qualified Data.UUID as U
import GHC.Generics

import Shared.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateDTO
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleDTO
import Wizard.Api.Resource.Questionnaire.QuestionnairePermDTO
import Wizard.Model.DocumentTemplate.DocumentTemplateState
import Wizard.Model.Questionnaire.Questionnaire

data QuestionnaireDetailSettings = QuestionnaireDetailSettings
  { uuid :: U.UUID
  , name :: String
  , description :: Maybe String
  , visibility :: QuestionnaireVisibility
  , sharing :: QuestionnaireSharing
  , isTemplate :: Bool
  , migrationUuid :: Maybe U.UUID
  , permissions :: [QuestionnairePermDTO]
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
