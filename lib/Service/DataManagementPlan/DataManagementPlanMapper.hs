module Service.DataManagementPlan.DataManagementPlanMapper where

import Control.Lens ((^.))

import Api.Resource.DataManagementPlan.DataManagementPlanDTO
import LensesConfig
import Model.DataManagementPlan.DataManagementPlan
import Service.KnowledgeModel.KnowledgeModelMapper
import Service.Level.LevelMapper
import Service.Metric.MetricMapper
import qualified Service.Organization.OrganizationMapper
       as ORG_Mapper
import Service.Package.PackageMapper
import qualified Service.Questionnaire.QuestionnaireMapper
       as QTN_Mapper
import Service.Report.ReportMapper
import qualified Service.User.UserMapper as USR_Mapper

toDataManagementPlanDTO :: DataManagementPlan -> DataManagementPlanDTO
toDataManagementPlanDTO dmp =
  DataManagementPlanDTO
  { _dataManagementPlanDTOUuid = dmp ^. uuid
  , _dataManagementPlanDTOConfig = toDataManagementPlanConfigDTO $ dmp ^. config
  , _dataManagementPlanDTOQuestionnaireUuid = dmp ^. questionnaireUuid
  , _dataManagementPlanDTOQuestionnaireName = dmp ^. questionnaireName
  , _dataManagementPlanDTOQuestionnaireReplies = QTN_Mapper.toReplyDTO <$> dmp ^. questionnaireReplies
  , _dataManagementPlanDTOLevel = dmp ^. level
  , _dataManagementPlanDTOKnowledgeModel = toKnowledgeModelDTO $ dmp ^. knowledgeModel
  , _dataManagementPlanDTOMetrics = toMetricDTO <$> dmp ^. metrics
  , _dataManagementPlanDTOLevels = toLevelDTO <$> dmp ^. levels
  , _dataManagementPlanDTOReport = toReportDTO $ dmp ^. report
  , _dataManagementPlanDTOPackage = toSimpleDTO (dmp ^. package)
  , _dataManagementPlanDTOOrganization = ORG_Mapper.toDTO $ dmp ^. organization
  , _dataManagementPlanDTOCreatedBy = USR_Mapper.toDTO <$> dmp ^. createdBy
  , _dataManagementPlanDTOCreatedAt = dmp ^. createdAt
  , _dataManagementPlanDTOUpdatedAt = dmp ^. updatedAt
  }

toDataManagementPlanConfigDTO :: DataManagementPlanConfig -> DataManagementPlanConfigDTO
toDataManagementPlanConfigDTO config =
  DataManagementPlanConfigDTO {_dataManagementPlanConfigDTOLevelsEnabled = config ^. levelsEnabled}
