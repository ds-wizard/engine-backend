module Wizard.Service.Report.ReportService where

import qualified Data.UUID as U

import Wizard.Api.Resource.Project.Detail.ProjectDetailQuestionnaireDTO
import Wizard.Api.Resource.Project.Detail.ProjectDetailReportDTO
import Wizard.Model.Context.AppContext
import Wizard.Service.KnowledgeModel.KnowledgeModelService
import Wizard.Service.Project.ProjectService
import Wizard.Service.Report.ReportGenerator
import Wizard.Service.Report.ReportMapper

getReportByProjectUuid :: U.UUID -> AppContextM ProjectDetailReportDTO
getReportByProjectUuid projectUuid = do
  projectDto <- getProjectDetailQuestionnaireByUuid projectUuid
  knowledgeModel <- compileKnowledgeModel [] (Just projectDto.knowledgeModelPackageId) projectDto.selectedQuestionTagUuids
  report <- generateReport projectDto.phaseUuid knowledgeModel projectDto.replies
  return $ toDTO projectDto report
