module Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDataDTO where

import qualified Data.UUID as U
import GHC.Generics

import Wizard.Model.Branch.BranchSuggestion
import Wizard.Model.Questionnaire.QuestionnaireSuggestion

data DocumentTemplateDraftDataDTO = DocumentTemplateDraftDataDTO
  { questionnaireUuid :: Maybe U.UUID
  , questionnaire :: Maybe QuestionnaireSuggestion
  , branchUuid :: Maybe U.UUID
  , branch :: Maybe BranchSuggestion
  , formatUuid :: Maybe U.UUID
  }
  deriving (Show, Eq, Generic)
