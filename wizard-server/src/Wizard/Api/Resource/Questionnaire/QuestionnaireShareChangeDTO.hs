module Wizard.Api.Resource.Questionnaire.QuestionnaireShareChangeDTO where

import GHC.Generics

import Wizard.Api.Resource.Questionnaire.QuestionnairePermChangeDTO
import Wizard.Model.Questionnaire.Questionnaire

data QuestionnaireShareChangeDTO = QuestionnaireShareChangeDTO
  { visibility :: QuestionnaireVisibility
  , sharing :: QuestionnaireSharing
  , permissions :: [QuestionnairePermChangeDTO]
  }
  deriving (Show, Eq, Generic)
