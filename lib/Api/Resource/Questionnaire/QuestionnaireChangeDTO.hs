module Api.Resource.Questionnaire.QuestionnaireChangeDTO where

import Api.Resource.Questionnaire.QuestionnaireReplyDTO

data QuestionnaireChangeDTO = QuestionnaireChangeDTO
  { _questionnaireChangeDTOName :: String
  , _questionnaireChangeDTOPrivate :: Bool
  , _questionnaireChangeDTOLevel :: Int
  , _questionnaireChangeDTOReplies :: [ReplyDTO]
  }
