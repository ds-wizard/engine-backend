module Api.Resource.DataManagementPlan.DataManagementPlanDTO where

import Control.Monad
import Data.Aeson
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Api.Resource.FilledKnowledgeModel.FilledKnowledgeModelDTO
import Api.Resource.KnowledgeModel.KnowledgeModelDTO
import Api.Resource.Level.LevelDTO
import Api.Resource.Level.LevelJS ()
import Api.Resource.Organization.OrganizationDTO
import Api.Resource.Package.PackageDTO
import Api.Resource.Package.PackageDTO ()
import Api.Resource.Report.ReportDTO
import Api.Resource.Report.ReportJM ()
import Api.Resource.User.UserDTO

data DataManagementPlanDTO = DataManagementPlanDTO
  { _dataManagementPlanDTOUuid :: U.UUID
  , _dataManagementPlanDTOQuestionnaireUuid :: String
  , _dataManagementPlanDTOLevel :: Int
  , _dataManagementPlanDTOFilledKnowledgeModel :: FilledKnowledgeModelDTO
  , _dataManagementPlanDTOMetrics :: [MetricDTO]
  , _dataManagementPlanDTOLevels :: [LevelDTO]
  , _dataManagementPlanDTOReport :: ReportDTO
  , _dataManagementPlanDTOPackage :: PackageDTO
  , _dataManagementPlanDTOOrganization :: OrganizationDTO
  , _dataManagementPlanDTOCreatedBy :: Maybe UserDTO
  , _dataManagementPlanDTOCreatedAt :: UTCTime
  , _dataManagementPlanDTOUpdatedAt :: UTCTime
  } deriving (Show, Generic)

instance Eq DataManagementPlanDTO where
  a == b =
    _dataManagementPlanDTOUuid a == _dataManagementPlanDTOUuid b &&
    _dataManagementPlanDTOQuestionnaireUuid a == _dataManagementPlanDTOQuestionnaireUuid b &&
    _dataManagementPlanDTOLevel a == _dataManagementPlanDTOLevel b &&
    _dataManagementPlanDTOFilledKnowledgeModel a == _dataManagementPlanDTOFilledKnowledgeModel b &&
    _dataManagementPlanDTOMetrics a == _dataManagementPlanDTOMetrics b &&
    _dataManagementPlanDTOLevels a == _dataManagementPlanDTOLevels b &&
    _dataManagementPlanDTOReport a == _dataManagementPlanDTOReport b &&
    _dataManagementPlanDTOPackage a == _dataManagementPlanDTOPackage b &&
    _dataManagementPlanDTOOrganization a == _dataManagementPlanDTOOrganization b &&
    _dataManagementPlanDTOCreatedBy a == _dataManagementPlanDTOCreatedBy b

instance FromJSON DataManagementPlanDTO where
  parseJSON (Object o) = do
    _dataManagementPlanDTOUuid <- o .: "uuid"
    _dataManagementPlanDTOQuestionnaireUuid <- o .: "questionnaireUuid"
    _dataManagementPlanDTOLevel <- o .: "level"
    _dataManagementPlanDTOFilledKnowledgeModel <- o .: "filledKnowledgeModel"
    _dataManagementPlanDTOMetrics <- o .: "metrics"
    _dataManagementPlanDTOLevels <- o .: "levels"
    _dataManagementPlanDTOReport <- o .: "report"
    _dataManagementPlanDTOPackage <- o .: "package"
    _dataManagementPlanDTOOrganization <- o .: "organization"
    _dataManagementPlanDTOCreatedBy <- o .: "createdBy"
    _dataManagementPlanDTOCreatedAt <- o .: "createdAt"
    _dataManagementPlanDTOUpdatedAt <- o .: "updatedAt"
    return DataManagementPlanDTO {..}
  parseJSON _ = mzero

instance ToJSON DataManagementPlanDTO where
  toJSON DataManagementPlanDTO {..} =
    object
      [ "uuid" .= _dataManagementPlanDTOUuid
      , "questionnaireUuid" .= _dataManagementPlanDTOQuestionnaireUuid
      , "level" .= _dataManagementPlanDTOLevel
      , "filledKnowledgeModel" .= _dataManagementPlanDTOFilledKnowledgeModel
      , "metrics" .= _dataManagementPlanDTOMetrics
      , "levels" .= _dataManagementPlanDTOLevels
      , "report" .= _dataManagementPlanDTOReport
      , "package" .= _dataManagementPlanDTOPackage
      , "organization" .= _dataManagementPlanDTOOrganization
      , "createdBy" .= _dataManagementPlanDTOCreatedBy
      , "createdAt" .= _dataManagementPlanDTOCreatedAt
      , "updatedAt" .= _dataManagementPlanDTOUpdatedAt
      ]
