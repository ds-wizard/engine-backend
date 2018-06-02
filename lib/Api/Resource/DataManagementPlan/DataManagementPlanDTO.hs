module Api.Resource.DataManagementPlan.DataManagementPlanDTO where

import Control.Monad
import Data.Aeson
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Api.Resource.FilledKnowledgeModel.FilledKnowledgeModelDTO

data DataManagementPlanDTO = DataManagementPlanDTO
  { _dataManagementPlanDTOUuid :: U.UUID
  , _dataManagementPlanDTOQuestionnaireUuid :: String
  , _dataManagementPlanDTOFilledKnowledgeModel :: FilledKnowledgeModelDTO
  , _dataManagementPlanDTOCreatedAt :: UTCTime
  , _dataManagementPlanDTOUpdatedAt :: UTCTime
  } deriving (Show, Generic)

instance Eq DataManagementPlanDTO where
  a == b =
    _dataManagementPlanDTOUuid a == _dataManagementPlanDTOUuid b &&
    _dataManagementPlanDTOQuestionnaireUuid a == _dataManagementPlanDTOQuestionnaireUuid b &&
    _dataManagementPlanDTOFilledKnowledgeModel a == _dataManagementPlanDTOFilledKnowledgeModel b

instance FromJSON DataManagementPlanDTO where
  parseJSON (Object o) = do
    _dataManagementPlanDTOUuid <- o .: "uuid"
    _dataManagementPlanDTOQuestionnaireUuid <- o .: "questionnaireUuid"
    _dataManagementPlanDTOFilledKnowledgeModel <- o .: "filledKnowledgeModel"
    _dataManagementPlanDTOCreatedAt <- o .: "createdAt"
    _dataManagementPlanDTOUpdatedAt <- o .: "updatedAt"
    return DataManagementPlanDTO {..}
  parseJSON _ = mzero

instance ToJSON DataManagementPlanDTO where
  toJSON DataManagementPlanDTO {..} =
    object
      [ "uuid" .= _dataManagementPlanDTOUuid
      , "questionnaireUuid" .= _dataManagementPlanDTOQuestionnaireUuid
      , "filledKnowledgeModel" .= _dataManagementPlanDTOFilledKnowledgeModel
      , "createdAt" .= _dataManagementPlanDTOCreatedAt
      , "updatedAt" .= _dataManagementPlanDTOUpdatedAt
      ]
