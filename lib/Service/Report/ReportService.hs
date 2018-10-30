module Service.Report.ReportService where

import Control.Lens ((&), (.~), (^.))
import Control.Monad.Reader (liftIO)

import Api.Resource.Questionnaire.QuestionnaireDetailDTO
import Api.Resource.Report.ReportDTO
import Database.DAO.Metric.MetricDAO
import Database.DAO.Questionnaire.QuestionnaireDAO
import LensesConfig
import Model.Context.AppContext
import Model.Error.Error
import Service.DataManagementPlan.DataManagementPlanService
import Service.Questionnaire.QuestionnaireMapper
import Service.Report.ReportGenerator
import Service.Report.ReportMapper

getReportByQuestionnaireUuid :: String -> AppContextM (Either AppError ReportDTO)
getReportByQuestionnaireUuid qtnUuid =
  heFindQuestionnaireById qtnUuid $ \qtn ->
    heFindMetrics $ \metrics -> do
      let filledKM = createFilledKM qtn
      report <- liftIO $ generateReport (qtn ^. level) metrics filledKM
      return . Right . toReportDTO $ report

getPreviewOfReportByQuestionnaireUuid :: String -> [QuestionnaireReplyDTO] -> AppContextM (Either AppError ReportDTO)
getPreviewOfReportByQuestionnaireUuid qtnUuid reqDto =
  heFindQuestionnaireById qtnUuid $ \qtn ->
    heFindMetrics $ \metrics -> do
      let qtnWithModifiedReplies = qtn & replies .~ (fromReplyDTO <$> reqDto)
      let filledKM = createFilledKM qtnWithModifiedReplies
      report <- liftIO $ generateReport (qtn ^. level) metrics filledKM
      return . Right . toReportDTO $ report
