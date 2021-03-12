module Wizard.Api.Resource.Questionnaire.QuestionnaireContentChangeDTO where

import GHC.Generics

import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventChangeDTO

data QuestionnaireContentChangeDTO =
  QuestionnaireContentChangeDTO
    { _questionnaireContentChangeDTOEvents :: [QuestionnaireEventChangeDTO]
    }
  deriving (Show, Eq, Generic)
