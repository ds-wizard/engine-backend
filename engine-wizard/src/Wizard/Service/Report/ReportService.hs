module Wizard.Service.Report.ReportService where

import Control.Lens ((^.))
import Control.Monad.Reader (liftIO)
import Data.Time

import LensesConfig
import Wizard.Api.Resource.Questionnaire.QuestionnaireChangeDTO
import Wizard.Database.DAO.Metric.MetricDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Report.Report
import Wizard.Service.Common.ACL
import Wizard.Service.KnowledgeModel.KnowledgeModelService
import Wizard.Service.Questionnaire.QuestionnaireMapper
import Wizard.Service.Questionnaire.QuestionnaireService
import Wizard.Service.Questionnaire.QuestionnaireUtils
import Wizard.Service.Report.ReportGenerator

getReportByQuestionnaireUuid :: String -> AppContextM Report
getReportByQuestionnaireUuid qtnUuid = do
  checkPermission _QTN_PERM
  qtnDto <- getQuestionnaireDetailById qtnUuid
  knowledgeModel <- compileKnowledgeModel [] (Just $ qtnDto ^. package . pId) (qtnDto ^. selectedTagUuids)
  metrics <- findMetrics
  generateReport (qtnDto ^. level) metrics knowledgeModel (fromReplyDTO <$> qtnDto ^. replies)

getPreviewOfReportByQuestionnaireUuid :: String -> QuestionnaireChangeDTO -> AppContextM Report
getPreviewOfReportByQuestionnaireUuid qtnUuid reqDto = do
  checkPermission _QTN_PERM
  qtnDto <- getQuestionnaireDetailById qtnUuid
  knowledgeModel <- compileKnowledgeModel [] (Just $ qtnDto ^. package . pId) (qtnDto ^. selectedTagUuids)
  metrics <- findMetrics
  currentUser <- getCurrentUser
  visibility <- extractVisibility reqDto
  now <- liftIO getCurrentTime
  let updatedQtn = fromChangeDTO qtnDto reqDto visibility (currentUser ^. uuid) now
  generateReport (reqDto ^. level) metrics knowledgeModel (updatedQtn ^. replies)
