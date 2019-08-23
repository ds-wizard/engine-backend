module Api.Resource.Questionnaire.QuestionnaireChangeDTO where

import GHC.Generics

import Api.Resource.Questionnaire.QuestionnaireLabelDTO
import Api.Resource.Questionnaire.QuestionnaireReplyDTO
import Model.Questionnaire.Questionnaire

data QuestionnaireChangeDTO = QuestionnaireChangeDTO
  { _questionnaireChangeDTOName :: String
  , _questionnaireChangeDTOAccessibility :: QuestionnaireAccessibility
  , _questionnaireChangeDTOLevel :: Int
  , _questionnaireChangeDTOReplies :: [ReplyDTO]
  , _questionnaireChangeDTOLabels :: [LabelDTO]
  } deriving (Show, Eq, Generic)
