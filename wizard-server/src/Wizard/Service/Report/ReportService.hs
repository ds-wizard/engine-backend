module Wizard.Service.Report.ReportService where

import qualified Data.UUID as U

import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailQuestionnaireDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailReportDTO
import Wizard.Model.Context.AppContext
import Wizard.Service.KnowledgeModel.KnowledgeModelService
import Wizard.Service.Questionnaire.QuestionnaireService
import Wizard.Service.Report.ReportGenerator
import Wizard.Service.Report.ReportMapper

getReportByQuestionnaireUuid :: U.UUID -> AppContextM QuestionnaireDetailReportDTO
getReportByQuestionnaireUuid qtnUuid = do
  qtnDto <- getQuestionnaireDetailQuestionnaireByUuid qtnUuid
  knowledgeModel <- compileKnowledgeModel [] (Just qtnDto.packageId) qtnDto.selectedQuestionTagUuids
  report <- generateReport qtnDto.phaseUuid knowledgeModel qtnDto.replies
  return $ toDTO qtnDto report
