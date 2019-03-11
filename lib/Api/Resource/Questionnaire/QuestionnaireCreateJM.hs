module Api.Resource.Questionnaire.QuestionnaireCreateJM where

import Control.Monad
import Data.Aeson

import Api.Resource.Questionnaire.QuestionnaireCreateDTO

instance FromJSON QuestionnaireCreateDTO where
  parseJSON (Object o) = do
    _questionnaireCreateDTOName <- o .: "name"
    _questionnaireCreateDTOPackageId <- o .: "packageId"
    _questionnaireCreateDTOPrivate <- o .: "private"
    _questionnaireCreateDTOTagUuids <- o .: "tagUuids"
    return QuestionnaireCreateDTO {..}
  parseJSON _ = mzero

instance ToJSON QuestionnaireCreateDTO where
  toJSON QuestionnaireCreateDTO {..} =
    object
      [ "name" .= _questionnaireCreateDTOName
      , "packageId" .= _questionnaireCreateDTOPackageId
      , "private" .= _questionnaireCreateDTOPrivate
      , "tagUuids" .= _questionnaireCreateDTOTagUuids
      ]
