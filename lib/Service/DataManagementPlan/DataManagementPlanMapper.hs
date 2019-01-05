module Service.DataManagementPlan.DataManagementPlanMapper where

import Control.Lens ((^.))

import Api.Resource.DataManagementPlan.DataManagementPlanDTO
import LensesConfig
import Model.DataManagementPlan.DataManagementPlan
import Service.FilledKnowledgeModel.FilledKnowledgeModelMapper
import Service.Level.LevelMapper
import Service.Metric.MetricMapper
import qualified Service.Organization.OrganizationMapper as OM
import Service.Package.PackageMapper
import Service.Report.ReportMapper
import qualified Service.User.UserMapper as UM

toDTO :: DataManagementPlan -> DataManagementPlanDTO
toDTO dmp =
  DataManagementPlanDTO
  { _dataManagementPlanDTOUuid = dmp ^. uuid
  , _dataManagementPlanDTOQuestionnaireUuid = dmp ^. questionnaireUuid
  , _dataManagementPlanDTOLevel = dmp ^. level
  , _dataManagementPlanDTOFilledKnowledgeModel = toFilledKMDTO $ dmp ^. filledKnowledgeModel
  , _dataManagementPlanDTOMetrics = toMetricDTO <$> dmp ^. metrics
  , _dataManagementPlanDTOLevels = toLevelDTO <$> dmp ^. levels
  , _dataManagementPlanDTOReport = toReportDTO $ dmp ^. report
  , _dataManagementPlanDTOPackage = packageToDTO $ dmp ^. package
  , _dataManagementPlanDTOOrganization = OM.toDTO $ dmp ^. organization
  , _dataManagementPlanDTOCreatedBy = UM.toDTO <$> dmp ^. createdBy
  , _dataManagementPlanDTOCreatedAt = dmp ^. createdAt
  , _dataManagementPlanDTOUpdatedAt = dmp ^. updatedAt
  }
