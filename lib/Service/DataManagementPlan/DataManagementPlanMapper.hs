module Service.DataManagementPlan.DataManagementPlanMapper where

import Control.Lens ((^.))

import Api.Resource.DataManagementPlan.DataManagementPlanDTO
import LensesConfig
import Model.DataManagementPlan.DataManagementPlan
import Service.FilledKnowledgeModel.FilledKnowledgeModelMapper
import Service.Metric.MetricMapper
import Service.Report.ReportMapper

toDTO :: DataManagementPlan -> DataManagementPlanDTO
toDTO dmp =
  DataManagementPlanDTO
  { _dataManagementPlanDTOUuid = dmp ^. uuid
  , _dataManagementPlanDTOQuestionnaireUuid = dmp ^. questionnaireUuid
  , _dataManagementPlanDTOLevel = dmp ^. level
  , _dataManagementPlanDTOFilledKnowledgeModel = toFilledKMDTO $ dmp ^. filledKnowledgeModel
  , _dataManagementPlanDTOMetrics = toMetricDTO <$> dmp ^. metrics
  , _dataManagementPlanDTOReport = toReportDTO $ dmp ^. report
  , _dataManagementPlanDTOCreatedAt = dmp ^. createdAt
  , _dataManagementPlanDTOUpdatedAt = dmp ^. updatedAt
  }
