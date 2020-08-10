module Wizard.Service.Report.ReportService where

import Control.Lens ((^.))

import LensesConfig
import Wizard.Api.Resource.Questionnaire.QuestionnaireContentChangeDTO
import Wizard.Database.DAO.Metric.MetricDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Report.Report
import Wizard.Service.Common.ACL
import Wizard.Service.KnowledgeModel.KnowledgeModelService
import Wizard.Service.Questionnaire.QuestionnaireMapper
import Wizard.Service.Questionnaire.QuestionnaireService
import Wizard.Service.Report.ReportGenerator

getReportByQuestionnaireUuid :: String -> AppContextM Report
getReportByQuestionnaireUuid qtnUuid = do
  checkPermission _QTN_PERM
  qtnDto <- getQuestionnaireDetailById qtnUuid
  knowledgeModel <- compileKnowledgeModel [] (Just $ qtnDto ^. package . pId) (qtnDto ^. selectedTagUuids)
  metrics <- findMetrics
  generateReport (qtnDto ^. level) metrics knowledgeModel (fromReplyDTO <$> qtnDto ^. replies)

getPreviewOfReportByQuestionnaireUuid :: String -> QuestionnaireContentChangeDTO -> AppContextM Report
getPreviewOfReportByQuestionnaireUuid qtnUuid reqDto = do
  qtnDto <- getQuestionnaireDetailById qtnUuid
  knowledgeModel <- compileKnowledgeModel [] (Just $ qtnDto ^. package . pId) (qtnDto ^. selectedTagUuids)
  metrics <- findMetrics
  let updatedReplies = fromReplyDTO <$> reqDto ^. replies
  generateReport (reqDto ^. level) metrics knowledgeModel updatedReplies
