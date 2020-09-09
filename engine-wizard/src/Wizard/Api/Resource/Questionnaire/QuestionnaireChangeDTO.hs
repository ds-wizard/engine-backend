module Wizard.Api.Resource.Questionnaire.QuestionnaireChangeDTO where

import GHC.Generics

import Wizard.Model.Questionnaire.Questionnaire

data QuestionnaireChangeDTO =
  QuestionnaireChangeDTO
    { _questionnaireChangeDTOName :: String
    , _questionnaireChangeDTOVisibility :: QuestionnaireVisibility
    , _questionnaireChangeDTOSharing :: QuestionnaireSharing
    , _questionnaireChangeDTOTemplateId :: Maybe String
    }
  deriving (Show, Eq, Generic)
