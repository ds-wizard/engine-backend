module Api.Resource.Questionnaire.QuestionnaireChangeJM where

import Control.Monad
import Data.Aeson

import Api.Resource.Questionnaire.QuestionnaireChangeDTO
import Api.Resource.Questionnaire.QuestionnaireReplyJS ()

instance FromJSON QuestionnaireChangeDTO where
  parseJSON (Object o) = do
    _questionnaireChangeDTOName <- o .: "name"
    _questionnaireChangeDTOPrivate <- o .: "private"
    _questionnaireChangeDTOLevel <- o .: "level"
    _questionnaireChangeDTOReplies <- o .: "replies"
    return QuestionnaireChangeDTO {..}
  parseJSON _ = mzero

instance ToJSON QuestionnaireChangeDTO where
  toJSON QuestionnaireChangeDTO {..} =
    object
      [ "name" .= _questionnaireChangeDTOName
      , "private" .= _questionnaireChangeDTOPrivate
      , "level" .= _questionnaireChangeDTOLevel
      , "replies" .= _questionnaireChangeDTOReplies
      ]
