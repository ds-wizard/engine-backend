module Api.Resource.Questionnaire.QuestionnaireCreateJM where

import Control.Monad
import Data.Aeson

import Api.Resource.Questionnaire.Common
import Api.Resource.Questionnaire.QuestionnaireCreateDTO

instance FromJSON QuestionnaireCreateDTO where
  parseJSON (Object o) = do
    _questionnaireCreateDTOName <- o .: "name"
    _questionnaireCreateDTOPackageId <- o .: "packageId"
    _questionnaireCreateDTOTagUuids <- o .: "tagUuids"
    hDeserializeQuestionnaireAccessibility o $ \_questionnaireCreateDTOAccessibility ->
      return QuestionnaireCreateDTO {..}
  parseJSON _ = mzero

instance ToJSON QuestionnaireCreateDTO where
  toJSON QuestionnaireCreateDTO {..} =
    object
      [ "name" .= _questionnaireCreateDTOName
      , "packageId" .= _questionnaireCreateDTOPackageId
      , "accessibility" .= serializeQuestionnaireAccessibility _questionnaireCreateDTOAccessibility
      , "tagUuids" .= _questionnaireCreateDTOTagUuids
      ]
