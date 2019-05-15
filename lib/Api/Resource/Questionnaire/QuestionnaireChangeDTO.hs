module Api.Resource.Questionnaire.QuestionnaireChangeDTO where

import Api.Resource.Questionnaire.QuestionnaireReplyDTO
import Model.Questionnaire.Questionnaire

data QuestionnaireChangeDTO = QuestionnaireChangeDTO
  { _questionnaireChangeDTOName :: String
  , _questionnaireChangeDTOAccessibility :: QuestionnaireAccessibility
  , _questionnaireChangeDTOLevel :: Int
  , _questionnaireChangeDTOReplies :: [ReplyDTO]
  }
