module Wizard.Api.Resource.Questionnaire.QuestionnaireHistoryDTO where

import GHC.Generics

import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventDTO
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionDTO

data QuestionnaireHistoryDTO =
  QuestionnaireHistoryDTO
    { _questionnaireHistoryDTOEvents :: [QuestionnaireEventDTO]
    , _questionnaireHistoryDTOVersions :: [QuestionnaireVersionDTO]
    }
  deriving (Show, Eq, Generic)
