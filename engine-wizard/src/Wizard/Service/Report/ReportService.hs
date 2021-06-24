module Wizard.Service.Report.ReportService where

import Control.Lens ((^.))
import qualified Data.Map.Strict as M

import LensesConfig
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Report.Report
import Wizard.Service.KnowledgeModel.KnowledgeModelService
import Wizard.Service.Questionnaire.QuestionnaireService
import Wizard.Service.Report.ReportGenerator

getReportByQuestionnaireUuid :: String -> AppContextM Report
getReportByQuestionnaireUuid qtnUuid =
  runInTransaction $ do
    qtnDto <- getQuestionnaireDetailById qtnUuid
    knowledgeModel <- compileKnowledgeModel [] (Just $ qtnDto ^. package . pId) (qtnDto ^. selectedTagUuids)
    generateReport (qtnDto ^. level) knowledgeModel (M.toList $ qtnDto ^. replies)
