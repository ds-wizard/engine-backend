module Api.Resource.Questionnaire.QuestionnaireDetailJM where

import Control.Monad
import Data.Aeson

import Api.Resource.KnowledgeModel.KnowledgeModelJM ()
import Api.Resource.Package.PackageSimpleJM ()
import Api.Resource.Questionnaire.Common
import Api.Resource.Questionnaire.QuestionnaireDetailDTO
import Api.Resource.Questionnaire.QuestionnaireReplyJS ()
import Api.Resource.Questionnaire.QuestionnaireStateJM ()

instance FromJSON QuestionnaireDetailDTO where
  parseJSON (Object o) = do
    _questionnaireDetailDTOUuid <- o .: "uuid"
    _questionnaireDetailDTOName <- o .: "name"
    _questionnaireDetailDTOLevel <- o .: "level"
    _questionnaireDetailDTOState <- o .: "state"
    _questionnaireDetailDTOPackage <- o .: "package"
    _questionnaireDetailDTOSelectedTagUuids <- o .: "selectedTagUuids"
    _questionnaireDetailDTOKnowledgeModel <- o .: "knowledgeModel"
    _questionnaireDetailDTOReplies <- o .: "replies"
    _questionnaireDetailDTOOwnerUuid <- o .: "ownerUuid"
    _questionnaireDetailDTOCreatedAt <- o .: "createdAt"
    _questionnaireDetailDTOUpdatedAt <- o .: "updatedAt"
    hDeserializeQuestionnaireAccessibility o $ \_questionnaireDetailDTOAccessibility ->
      return QuestionnaireDetailDTO {..}
  parseJSON _ = mzero

instance ToJSON QuestionnaireDetailDTO where
  toJSON QuestionnaireDetailDTO {..} =
    object
      [ "uuid" .= _questionnaireDetailDTOUuid
      , "name" .= _questionnaireDetailDTOName
      , "level" .= _questionnaireDetailDTOLevel
      , "accessibility" .= serializeQuestionnaireAccessibility _questionnaireDetailDTOAccessibility
      , "state" .= _questionnaireDetailDTOState
      , "package" .= _questionnaireDetailDTOPackage
      , "selectedTagUuids" .= _questionnaireDetailDTOSelectedTagUuids
      , "knowledgeModel" .= _questionnaireDetailDTOKnowledgeModel
      , "replies" .= _questionnaireDetailDTOReplies
      , "ownerUuid" .= _questionnaireDetailDTOOwnerUuid
      , "createdAt" .= _questionnaireDetailDTOCreatedAt
      , "updatedAt" .= _questionnaireDetailDTOUpdatedAt
      ]
