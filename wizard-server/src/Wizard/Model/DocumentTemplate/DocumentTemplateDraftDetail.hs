module Wizard.Model.DocumentTemplate.DocumentTemplateDraftDetail where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackagePattern
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditorSuggestion
import Wizard.Model.Questionnaire.QuestionnaireSuggestion

data DocumentTemplateDraftDetail = DocumentTemplateDraftDetail
  { tId :: String
  , name :: String
  , templateId :: String
  , version :: String
  , description :: String
  , readme :: String
  , license :: String
  , allowedPackages :: [KnowledgeModelPackagePattern]
  , formats :: [DocumentTemplateFormat]
  , questionnaireUuid :: Maybe U.UUID
  , questionnaire :: Maybe QuestionnaireSuggestion
  , knowledgeModelEditorUuid :: Maybe U.UUID
  , knowledgeModelEditor :: Maybe KnowledgeModelEditorSuggestion
  , formatUuid :: Maybe U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
