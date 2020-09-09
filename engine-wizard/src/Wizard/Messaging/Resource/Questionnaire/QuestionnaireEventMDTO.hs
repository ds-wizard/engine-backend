module Wizard.Messaging.Resource.Questionnaire.QuestionnaireEventMDTO where

import GHC.Generics

import Wizard.Api.Resource.Questionnaire.QuestionnaireEventDTO

data QuestionnaireEventMDTO =
  QuestionnaireEventMDTO
    { _questionnaireEventMDTOQuestionnaireUuid :: String
    , _questionnaireEventMDTOEvent :: QuestionnaireEventDTO
    }
  deriving (Show, Generic)
