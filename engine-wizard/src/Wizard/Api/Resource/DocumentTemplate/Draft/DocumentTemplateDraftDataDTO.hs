module Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDataDTO where

import qualified Data.UUID as U
import GHC.Generics

import Wizard.Model.Questionnaire.QuestionnaireSuggestion

data DocumentTemplateDraftDataDTO = DocumentTemplateDraftDataDTO
  { questionnaireUuid :: Maybe U.UUID
  , formatUuid :: Maybe U.UUID
  , questionnaire :: Maybe QuestionnaireSuggestion
  }
  deriving (Show, Eq, Generic)
