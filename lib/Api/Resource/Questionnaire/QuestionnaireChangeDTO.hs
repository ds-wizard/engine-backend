module Api.Resource.Questionnaire.QuestionnaireChangeDTO where

import Api.Resource.Questionnaire.QuestionnaireDetailDTO

data QuestionnaireChangeDTO = QuestionnaireChangeDTO
  { _questionnaireChangeDTOLevel :: Int
  , _questionnaireChangeDTOReplies :: [QuestionnaireReplyDTO]
  }
