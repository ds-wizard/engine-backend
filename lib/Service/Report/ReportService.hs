module Service.Report.ReportService where

import Control.Lens ((^.))
import Control.Monad.Reader (liftIO)
import Data.Time

import Api.Resource.Questionnaire.QuestionnaireChangeDTO
import Api.Resource.Report.ReportDTO
import Database.DAO.Metric.MetricDAO
import Database.DAO.Questionnaire.QuestionnaireDAO
import LensesConfig
import Model.Context.AppContext
import Model.Error.Error
import Service.DataManagementPlan.DataManagementPlanService
import Service.Questionnaire.QuestionnaireMapper
import Service.Questionnaire.QuestionnaireService
import Service.Report.ReportGenerator
import Service.Report.ReportMapper

getReportByQuestionnaireUuid :: String -> AppContextM (Either AppError ReportDTO)
getReportByQuestionnaireUuid qtnUuid =
  heFindQuestionnaireById qtnUuid $ \qtn ->
    heFindMetrics $ \metrics -> do
      let filledKM = createFilledKM qtn
      report <- generateReport (qtn ^. level) metrics filledKM
      return . Right . toReportDTO $ report

getPreviewOfReportByQuestionnaireUuid :: String -> QuestionnaireChangeDTO -> AppContextM (Either AppError ReportDTO)
getPreviewOfReportByQuestionnaireUuid qtnUuid reqDto =
  heGetQuestionnaireDetailById qtnUuid $ \qtnDto ->
    heFindMetrics $ \metrics -> do
      now <- liftIO getCurrentTime
      let updatedQtn = fromChangeDTO qtnDto reqDto now
      let filledKM = createFilledKM updatedQtn
      report <- generateReport (reqDto ^. level) metrics filledKM
      return . Right . toReportDTO $ report
