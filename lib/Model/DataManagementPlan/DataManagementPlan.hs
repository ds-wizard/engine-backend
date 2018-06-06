module Model.DataManagementPlan.DataManagementPlan where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Model.FilledKnowledgeModel.FilledKnowledgeModel

data DataManagementPlanFormat
  = JSON
  | HTML
  deriving (Show, Eq, Generic)

data DataManagementPlan = DataManagementPlan
  { _dataManagementPlanUuid :: U.UUID
  , _dataManagementPlanQuestionnaireUuid :: String
  , _dataManagementPlanFilledKnowledgeModel :: FilledKnowledgeModel
  , _dataManagementPlanCreatedAt :: UTCTime
  , _dataManagementPlanUpdatedAt :: UTCTime
  } deriving (Show, Generic)

instance Eq DataManagementPlan where
  a == b =
    _dataManagementPlanUuid a == _dataManagementPlanUuid b &&
    _dataManagementPlanQuestionnaireUuid a == _dataManagementPlanQuestionnaireUuid b &&
    _dataManagementPlanFilledKnowledgeModel a == _dataManagementPlanFilledKnowledgeModel b
