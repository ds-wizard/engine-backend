module Wizard.Api.Resource.Questionnaire.QuestionnaireChangeDTO where

import GHC.Generics

import Wizard.Api.Resource.Questionnaire.QuestionnaireLabelDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplyDTO
import Wizard.Model.Questionnaire.Questionnaire

data QuestionnaireChangeDTO =
  QuestionnaireChangeDTO
    { _questionnaireChangeDTOName :: String
    , _questionnaireChangeDTOAccessibility :: QuestionnaireAccessibility
    , _questionnaireChangeDTOLevel :: Int
    , _questionnaireChangeDTOReplies :: [ReplyDTO]
    , _questionnaireChangeDTOLabels :: [LabelDTO]
    }
  deriving (Show, Eq, Generic)
