module Api.Resource.Questionnaire.QuestionnaireChangeJM where

import Control.Monad
import Data.Aeson

import Api.Resource.Questionnaire.QuestionnaireChangeDTO

instance FromJSON QuestionnaireChangeDTO where
  parseJSON (Object o) = do
    _questionnaireChangeDTOLevel <- o .: "level"
    _questionnaireChangeDTOReplies <- o .: "replies"
    return QuestionnaireChangeDTO {..}
  parseJSON _ = mzero

instance ToJSON QuestionnaireChangeDTO where
  toJSON QuestionnaireChangeDTO {..} =
    object ["level" .= _questionnaireChangeDTOLevel, "replies" .= _questionnaireChangeDTOReplies]
