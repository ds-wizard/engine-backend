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

toDataManagementPlanDTO :: DataManagementPlan -> DataManagementPlanDTO
toDataManagementPlanDTO dmp =
  DataManagementPlanDTO
  { _dataManagementPlanDTOUuid = dmp ^. uuid
  , _dataManagementPlanDTOConfig = toDataManagementPlanConfigDTO $ dmp ^. config
  , _dataManagementPlanDTOQuestionnaireUuid = dmp ^. questionnaireUuid
  , _dataManagementPlanDTOQuestionnaireName = dmp ^. questionnaireName
  , _dataManagementPlanDTOLevel = dmp ^. level
  , _dataManagementPlanDTOFilledKnowledgeModel = toFilledKMDTO $ dmp ^. filledKnowledgeModel
  , _dataManagementPlanDTOMetrics = toMetricDTO <$> dmp ^. metrics
  , _dataManagementPlanDTOLevels = toLevelDTO <$> dmp ^. levels
  , _dataManagementPlanDTOReport = toReportDTO $ dmp ^. report
  , _dataManagementPlanDTOPackage = toSimpleDTO (dmp ^. package)
  , _dataManagementPlanDTOOrganization = OM.toDTO $ dmp ^. organization
  , _dataManagementPlanDTOCreatedBy = UM.toDTO <$> dmp ^. createdBy
  , _dataManagementPlanDTOCreatedAt = dmp ^. createdAt
  , _dataManagementPlanDTOUpdatedAt = dmp ^. updatedAt
  }

toDataManagementPlanConfigDTO :: DataManagementPlanConfig -> DataManagementPlanConfigDTO
toDataManagementPlanConfigDTO config =
  DataManagementPlanConfigDTO
  { _dataManagementPlanConfigDTOLevelsEnabled = config ^. levelsEnabled
  , _dataManagementPlanConfigDTOItemTitleEnabled = config ^. itemTitleEnabled
  }
