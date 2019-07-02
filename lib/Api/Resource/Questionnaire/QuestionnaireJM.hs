module Api.Resource.Questionnaire.QuestionnaireJM where

import Control.Monad
import Data.Aeson

import Api.Resource.Package.PackageSimpleJM ()
import Api.Resource.Questionnaire.Common
import Api.Resource.Questionnaire.QuestionnaireDTO
import Api.Resource.Questionnaire.QuestionnaireStateJM ()

instance FromJSON QuestionnaireDTO where
  parseJSON (Object o) = do
    _questionnaireDTOUuid <- o .: "uuid"
    _questionnaireDTOName <- o .: "name"
    _questionnaireDTOLevel <- o .: "level"
    _questionnaireDTOState <- o .: "state"
    _questionnaireDTOPackage <- o .: "package"
    _questionnaireDTOOwnerUuid <- o .: "ownerUuid"
    _questionnaireDTOCreatedAt <- o .: "createdAt"
    _questionnaireDTOUpdatedAt <- o .: "updatedAt"
    hDeserializeQuestionnaireAccessibility o $ \_questionnaireDTOAccessibility -> return QuestionnaireDTO {..}
  parseJSON _ = mzero

instance ToJSON QuestionnaireDTO where
  toJSON QuestionnaireDTO {..} =
    object
      [ "uuid" .= _questionnaireDTOUuid
      , "name" .= _questionnaireDTOName
      , "level" .= _questionnaireDTOLevel
      , "accessibility" .= serializeQuestionnaireAccessibility _questionnaireDTOAccessibility
      , "state" .= _questionnaireDTOState
      , "package" .= _questionnaireDTOPackage
      , "ownerUuid" .= _questionnaireDTOOwnerUuid
      , "createdAt" .= _questionnaireDTOCreatedAt
      , "updatedAt" .= _questionnaireDTOUpdatedAt
      ]
