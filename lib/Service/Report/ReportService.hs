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
import Model.Context.AppContextHelpers
import Model.Error.Error
import Service.DataManagementPlan.DataManagementPlanService
import Service.KnowledgeModel.KnowledgeModelService
import Service.Questionnaire.QuestionnaireMapper
import Service.Questionnaire.QuestionnaireService
import Service.Report.ReportGenerator
import Service.Report.ReportMapper

getReportByQuestionnaireUuid :: String -> AppContextM (Either AppError ReportDTO)
getReportByQuestionnaireUuid qtnUuid =
  heFindQuestionnaireById qtnUuid $ \qtn ->
    heCompileKnowledgeModel [] (Just $ qtn ^. packageId) (qtn ^. selectedTagUuids) $ \knowledgeModel ->
      heFindMetrics $ \metrics -> do
        let filledKM = createFilledKM knowledgeModel (qtn ^. replies)
        report <- generateReport (qtn ^. level) metrics filledKM
        return . Right . toReportDTO $ report

getPreviewOfReportByQuestionnaireUuid :: String -> QuestionnaireChangeDTO -> AppContextM (Either AppError ReportDTO)
getPreviewOfReportByQuestionnaireUuid qtnUuid reqDto =
  heGetQuestionnaireDetailById qtnUuid $ \qtnDto ->
    heCompileKnowledgeModel [] (Just $ qtnDto ^. package . pId) (qtnDto ^. selectedTagUuids) $ \knowledgeModel ->
      heGetCurrentUser $ \currentUser ->
        heFindMetrics $ \metrics -> do
          now <- liftIO getCurrentTime
          let updatedQtn = fromChangeDTO qtnDto reqDto (currentUser ^. uuid) now
          let filledKM = createFilledKM knowledgeModel (updatedQtn ^. replies)
          report <- generateReport (reqDto ^. level) metrics filledKM
          return . Right . toReportDTO $ report
