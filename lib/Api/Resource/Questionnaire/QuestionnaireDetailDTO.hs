module Api.Resource.Questionnaire.QuestionnaireDetailDTO where

import Control.Monad
import Data.Aeson
import Data.Time
import qualified Data.UUID as U

import Api.Resource.KnowledgeModel.KnowledgeModelDTO
import Api.Resource.Package.PackageDTO

data QuestionnaireReplyDTO = QuestionnaireReplyDTO
  { _questionnaireReplyDTOPath :: String
  , _questionnaireReplyDTOValue :: String
  } deriving (Show, Eq)

data QuestionnaireDetailDTO = QuestionnaireDetailDTO
  { _questionnaireDetailDTOUuid :: U.UUID
  , _questionnaireDetailDTOName :: String
  , _questionnaireDetailDTOLevel :: Int
  , _questionnaireDetailDTOPrivate :: Bool
  , _questionnaireDetailDTOPackage :: PackageDTO
  , _questionnaireDetailDTOKnowledgeModel :: KnowledgeModelDTO
  , _questionnaireDetailDTOReplies :: [QuestionnaireReplyDTO]
  , _questionnaireDetailDTOOwnerUuid :: Maybe U.UUID
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
    _questionnaireDetailDTOPrivate <- o .: "private"
    _questionnaireDetailDTOPackage <- o .: "package"
    _questionnaireDetailDTOKnowledgeModel <- o .: "knowledgeModel"
    _questionnaireDetailDTOReplies <- o .: "replies"
    _questionnaireDetailDTOOwnerUuid <- o .: "ownerUuid"
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
      , "private" .= _questionnaireDetailDTOPrivate
      , "package" .= _questionnaireDetailDTOPackage
      , "knowledgeModel" .= _questionnaireDetailDTOKnowledgeModel
      , "replies" .= _questionnaireDetailDTOReplies
      , "ownerUuid" .= _questionnaireDetailDTOOwnerUuid
      , "createdAt" .= _questionnaireDetailDTOCreatedAt
      , "updatedAt" .= _questionnaireDetailDTOUpdatedAt
      ]
