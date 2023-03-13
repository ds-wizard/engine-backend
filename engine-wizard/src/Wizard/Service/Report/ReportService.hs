module Wizard.Service.Report.ReportService where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailDTO
import Wizard.Model.Context.AppContext
import Wizard.Model.Report.Report
import Wizard.Service.KnowledgeModel.KnowledgeModelService
import Wizard.Service.Questionnaire.QuestionnaireService
import Wizard.Service.Report.ReportGenerator

getReportByQuestionnaireUuid :: U.UUID -> AppContextM Report
getReportByQuestionnaireUuid qtnUuid = do
  qtnDto <- getQuestionnaireDetailById qtnUuid
  knowledgeModel <- compileKnowledgeModel [] (Just qtnDto.package.pId) qtnDto.selectedQuestionTagUuids
  generateReport qtnDto.phaseUuid knowledgeModel (M.toList qtnDto.replies)
