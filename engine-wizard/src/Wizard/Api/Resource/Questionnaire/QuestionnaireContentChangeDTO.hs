module Wizard.Api.Resource.Questionnaire.QuestionnaireContentChangeDTO where

import GHC.Generics

import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventChangeDTO

data QuestionnaireContentChangeDTO = QuestionnaireContentChangeDTO
  { events :: [QuestionnaireEventChangeDTO]
  }
  deriving (Show, Eq, Generic)
