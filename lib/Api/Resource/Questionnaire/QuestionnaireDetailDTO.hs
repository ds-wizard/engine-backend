module Api.Resource.Questionnaire.QuestionnaireDetailDTO where

import Control.Monad
import Data.Aeson
import Data.Time
import Data.UUID

import Api.Resource.KnowledgeModel.KnowledgeModelDTO
import Api.Resource.Package.PackageDTO

data QuestionnaireReplyDTO = QuestionnaireReplyDTO
  { _questionnaireReplyDTOPath :: String
  , _questionnaireReplyDTOValue :: String
  } deriving (Show, Eq)

data QuestionnaireDetailDTO = QuestionnaireDetailDTO
  { _questionnaireDetailDTOUuid :: UUID
  , _questionnaireDetailDTOName :: String
  , _questionnaireDetailDTOLevel :: Int
  , _questionnaireDetailDTOPackage :: PackageDTO
  , _questionnaireDetailDTOKnowledgeModel :: KnowledgeModelDTO
  , _questionnaireDetailDTOReplies :: [QuestionnaireReplyDTO]
  , _questionnaireDetailDTOCreatedAt :: UTCTime
  , _questionnaireDetailDTOUpdatedAt :: UTCTime
  } deriving (Show, Eq)

instance FromJSON QuestionnaireReplyDTO where
  parseJSON (Object o) = do
    _questionnaireReplyDTOPath <- o .: "path"
    _questionnaireReplyDTOValue <- o .: "value"
    return QuestionnaireReplyDTO {..}
  parseJSON _ = mzero

instance ToJSON QuestionnaireReplyDTO where
  toJSON QuestionnaireReplyDTO {..} =
    object ["path" .= _questionnaireReplyDTOPath, "value" .= _questionnaireReplyDTOValue]

instance FromJSON QuestionnaireDetailDTO where
  parseJSON (Object o) = do
    _questionnaireDetailDTOUuid <- o .: "uuid"
    _questionnaireDetailDTOName <- o .: "name"
    _questionnaireDetailDTOLevel <- o .: "level"
    _questionnaireDetailDTOPackage <- o .: "package"
    _questionnaireDetailDTOKnowledgeModel <- o .: "knowledgeModel"
    _questionnaireDetailDTOReplies <- o .: "replies"
    _questionnaireDetailDTOCreatedAt <- o .: "createdAt"
    _questionnaireDetailDTOUpdatedAt <- o .: "updatedAt"
    return QuestionnaireDetailDTO {..}
  parseJSON _ = mzero

instance ToJSON QuestionnaireDetailDTO where
  toJSON QuestionnaireDetailDTO {..} =
    object
      [ "uuid" .= _questionnaireDetailDTOUuid
      , "name" .= _questionnaireDetailDTOName
      , "level" .= _questionnaireDetailDTOLevel
      , "package" .= _questionnaireDetailDTOPackage
      , "knowledgeModel" .= _questionnaireDetailDTOKnowledgeModel
      , "replies" .= _questionnaireDetailDTOReplies
      , "createdAt" .= _questionnaireDetailDTOCreatedAt
      , "updatedAt" .= _questionnaireDetailDTOUpdatedAt
      ]
