module Wizard.Model.Questionnaire.QuestionnaireDetailSettings where

import qualified Data.UUID as U
import GHC.Generics

import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Api.Resource.Questionnaire.QuestionnairePermDTO
import Wizard.Model.DocumentTemplate.DocumentTemplateState
import Wizard.Model.Questionnaire.Questionnaire
import WizardLib.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateDTO
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel

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
  , packageId :: String
  , package :: PackageSimpleDTO
  , knowledgeModelTags :: [Tag]
  , documentTemplate :: Maybe DocumentTemplateDTO
  , documentTemplateState :: Maybe DocumentTemplateState
  , documentTemplatePhase :: Maybe DocumentTemplatePhase
  , formatUuid :: Maybe U.UUID
  , selectedQuestionTagUuids :: [U.UUID]
  , fileCount :: Int
  }
  deriving (Show, Eq, Generic)
