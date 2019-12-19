module Wizard.Service.Report.ReportService where

import Control.Lens ((^.))
import Control.Monad.Reader (liftIO)
import Data.Time

import Shared.Model.Error.Error
import Wizard.Api.Resource.Questionnaire.QuestionnaireChangeDTO
import Wizard.Api.Resource.Report.ReportDTO
import Wizard.Database.DAO.Metric.MetricDAO
import Wizard.LensesConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Service.KnowledgeModel.KnowledgeModelService
import Wizard.Service.Questionnaire.QuestionnaireMapper
import Wizard.Service.Questionnaire.QuestionnaireService
import Wizard.Service.Report.ReportGenerator
import Wizard.Service.Report.ReportMapper

getReportByQuestionnaireUuid :: String -> AppContextM (Either AppError ReportDTO)
getReportByQuestionnaireUuid qtnUuid =
  heGetQuestionnaireDetailById qtnUuid $ \qtnDto ->
    heCompileKnowledgeModel [] (Just $ qtnDto ^. package . pId) (qtnDto ^. selectedTagUuids) $ \knowledgeModel ->
      heFindMetrics $ \metrics -> do
        report <- generateReport (qtnDto ^. level) metrics knowledgeModel (fromReplyDTO <$> qtnDto ^. replies)
        return . Right . toReportDTO $ report

getPreviewOfReportByQuestionnaireUuid :: String -> QuestionnaireChangeDTO -> AppContextM (Either AppError ReportDTO)
getPreviewOfReportByQuestionnaireUuid qtnUuid reqDto =
  heGetQuestionnaireDetailById qtnUuid $ \qtnDto ->
    heCompileKnowledgeModel [] (Just $ qtnDto ^. package . pId) (qtnDto ^. selectedTagUuids) $ \knowledgeModel ->
      heGetCurrentUser $ \currentUser ->
        heFindMetrics $ \metrics -> do
          now <- liftIO getCurrentTime
          accessibility <- extractAccessibility reqDto
          let updatedQtn = fromChangeDTO qtnDto reqDto accessibility (currentUser ^. uuid) now
          report <- generateReport (reqDto ^. level) metrics knowledgeModel (updatedQtn ^. replies)
          return . Right . toReportDTO $ report
