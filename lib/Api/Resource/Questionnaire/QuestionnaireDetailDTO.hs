module Api.Resource.Questionnaire.QuestionnaireDetailDTO where

import Control.Lens ((^.), makeLenses)
import Control.Monad

import Data.Aeson
import Data.Text
import Data.UUID

import Api.Resource.KnowledgeModel.KnowledgeModelDTO
import Api.Resource.Package.PackageDTO
import Common.Types
import Common.Uuid
import Model.Questionnaire.Questionnaire

data QuestionnaireDetailDTO = QuestionnaireDetailDTO
  { _questionnaireDetailDTOUuid :: UUID
  , _questionnaireDetailDTOName :: String
  , _questionnaireDetailDTOPackage :: PackageDTO
  , _questionnaireDetailDTOKnowledgeModel :: KnowledgeModelDTO
  , _questionnaireDetailDTOReplies :: QuestionnaireReplies
  } deriving (Show, Eq)

instance FromJSON QuestionnaireDetailDTO where
  parseJSON (Object o) = do
    _questionnaireDetailDTOUuid <- o .: "uuid"
    _questionnaireDetailDTOName <- o .: "name"
    _questionnaireDetailDTOPackage <- o .: "package"
    _questionnaireDetailDTOKnowledgeModel <- o .: "knowledgeModel"
    _questionnaireDetailDTOReplies <- o .: "replies"
    return QuestionnaireDetailDTO {..}
  parseJSON _ = mzero

instance ToJSON QuestionnaireDetailDTO where
  toJSON QuestionnaireDetailDTO {..} =
    object
      [ "uuid" .= _questionnaireDetailDTOUuid
      , "name" .= _questionnaireDetailDTOName
      , "package" .= _questionnaireDetailDTOPackage
      , "knowledgeModel" .= _questionnaireDetailDTOKnowledgeModel
      , "replies" .= _questionnaireDetailDTOReplies
      ]
