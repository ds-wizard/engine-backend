module Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDataDTO where

import qualified Data.UUID as U
import GHC.Generics

import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditorSuggestion
import Wizard.Model.Questionnaire.QuestionnaireSuggestion

data DocumentTemplateDraftDataDTO = DocumentTemplateDraftDataDTO
  { questionnaireUuid :: Maybe U.UUID
  , questionnaire :: Maybe QuestionnaireSuggestion
  , knowledgeModelEditorUuid :: Maybe U.UUID
  , knowledgeModelEditor :: Maybe KnowledgeModelEditorSuggestion
  , formatUuid :: Maybe U.UUID
  }
  deriving (Show, Eq, Generic)
