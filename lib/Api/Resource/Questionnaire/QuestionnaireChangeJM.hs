module Api.Resource.Questionnaire.QuestionnaireChangeJM where

import Control.Monad
import Data.Aeson

import Api.Resource.Questionnaire.Common
import Api.Resource.Questionnaire.QuestionnaireChangeDTO
import Api.Resource.Questionnaire.QuestionnaireReplyJS ()

instance FromJSON QuestionnaireChangeDTO where
  parseJSON (Object o) = do
    _questionnaireChangeDTOName <- o .: "name"
    _questionnaireChangeDTOLevel <- o .: "level"
    _questionnaireChangeDTOReplies <- o .: "replies"
    hDeserializeQuestionnaireAccessibility o $ \_questionnaireChangeDTOAccessibility ->
      return QuestionnaireChangeDTO {..}
  parseJSON _ = mzero

instance ToJSON QuestionnaireChangeDTO where
  toJSON QuestionnaireChangeDTO {..} =
    object
      [ "name" .= _questionnaireChangeDTOName
      , "accessibility" .= serializeQuestionnaireAccessibility _questionnaireChangeDTOAccessibility
      , "level" .= _questionnaireChangeDTOLevel
      , "replies" .= _questionnaireChangeDTOReplies
      ]
