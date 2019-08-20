module Api.Resource.DataManagementPlan.DataManagementPlanDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Api.Resource.KnowledgeModel.KnowledgeModelDTO
import Api.Resource.Level.LevelDTO
import Api.Resource.Organization.OrganizationDTO
import Api.Resource.Package.PackageSimpleDTO
import Api.Resource.Questionnaire.QuestionnaireReplyDTO
import Api.Resource.Report.ReportDTO
import Api.Resource.User.UserDTO

data DataManagementPlanDTO = DataManagementPlanDTO
  { _dataManagementPlanDTOUuid :: U.UUID
  , _dataManagementPlanDTOConfig :: DataManagementPlanConfigDTO
  , _dataManagementPlanDTOQuestionnaireUuid :: String
  , _dataManagementPlanDTOQuestionnaireName :: String
  , _dataManagementPlanDTOQuestionnaireReplies :: [ReplyDTO]
  , _dataManagementPlanDTOLevel :: Int
  , _dataManagementPlanDTOKnowledgeModel :: KnowledgeModelDTO
  , _dataManagementPlanDTOMetrics :: [MetricDTO]
  , _dataManagementPlanDTOLevels :: [LevelDTO]
  , _dataManagementPlanDTOReport :: ReportDTO
  , _dataManagementPlanDTOPackage :: PackageSimpleDTO
  , _dataManagementPlanDTOOrganization :: OrganizationDTO
  , _dataManagementPlanDTOCreatedBy :: Maybe UserDTO
  , _dataManagementPlanDTOCreatedAt :: UTCTime
  , _dataManagementPlanDTOUpdatedAt :: UTCTime
  } deriving (Show, Generic)

instance Eq DataManagementPlanDTO where
  a == b =
    _dataManagementPlanDTOUuid a == _dataManagementPlanDTOUuid b &&
    _dataManagementPlanDTOConfig a == _dataManagementPlanDTOConfig b &&
    _dataManagementPlanDTOQuestionnaireUuid a == _dataManagementPlanDTOQuestionnaireUuid b &&
    _dataManagementPlanDTOQuestionnaireName a == _dataManagementPlanDTOQuestionnaireName b &&
    _dataManagementPlanDTOQuestionnaireReplies a == _dataManagementPlanDTOQuestionnaireReplies b &&
    _dataManagementPlanDTOLevel a == _dataManagementPlanDTOLevel b &&
    _dataManagementPlanDTOKnowledgeModel a == _dataManagementPlanDTOKnowledgeModel b &&
    _dataManagementPlanDTOMetrics a == _dataManagementPlanDTOMetrics b &&
    _dataManagementPlanDTOLevels a == _dataManagementPlanDTOLevels b &&
    _dataManagementPlanDTOReport a == _dataManagementPlanDTOReport b &&
    _dataManagementPlanDTOPackage a == _dataManagementPlanDTOPackage b &&
    _dataManagementPlanDTOOrganization a == _dataManagementPlanDTOOrganization b &&
    _dataManagementPlanDTOCreatedBy a == _dataManagementPlanDTOCreatedBy b

data DataManagementPlanConfigDTO = DataManagementPlanConfigDTO
  { _dataManagementPlanConfigDTOLevelsEnabled :: Bool
  , _dataManagementPlanConfigDTOItemTitleEnabled :: Bool
  } deriving (Show, Eq, Generic)
