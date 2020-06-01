module Wizard.Service.Report.ReportService where

import Control.Lens ((^.))
import Control.Monad.Reader (liftIO)
import Data.Time

import LensesConfig
import Wizard.Api.Resource.Questionnaire.QuestionnaireChangeDTO
import Wizard.Api.Resource.Report.ReportDTO
import Wizard.Database.DAO.Metric.MetricDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Service.KnowledgeModel.KnowledgeModelService
import Wizard.Service.Questionnaire.QuestionnaireMapper
import Wizard.Service.Questionnaire.QuestionnaireService
import Wizard.Service.Report.ReportGenerator
import Wizard.Service.Report.ReportMapper

getReportByQuestionnaireUuid :: String -> AppContextM ReportDTO
getReportByQuestionnaireUuid qtnUuid = do
  qtnDto <- getQuestionnaireDetailById qtnUuid
  knowledgeModel <- compileKnowledgeModel [] (Just $ qtnDto ^. package . pId) (qtnDto ^. selectedTagUuids)
  metrics <- findMetrics
  report <- generateReport (qtnDto ^. level) metrics knowledgeModel (fromReplyDTO <$> qtnDto ^. replies)
  return . toReportDTO $ report

getPreviewOfReportByQuestionnaireUuid :: String -> QuestionnaireChangeDTO -> AppContextM ReportDTO
getPreviewOfReportByQuestionnaireUuid qtnUuid reqDto = do
  qtnDto <- getQuestionnaireDetailById qtnUuid
  knowledgeModel <- compileKnowledgeModel [] (Just $ qtnDto ^. package . pId) (qtnDto ^. selectedTagUuids)
  currentUser <- getCurrentUser
  metrics <- findMetrics
  now <- liftIO getCurrentTime
  visibility <- extractVisibility reqDto
  let updatedQtn = fromChangeDTO qtnDto reqDto visibility (currentUser ^. uuid) now
  report <- generateReport (reqDto ^. level) metrics knowledgeModel (updatedQtn ^. replies)
  return . toReportDTO $ report
