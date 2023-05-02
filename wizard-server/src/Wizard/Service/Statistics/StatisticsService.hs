module Wizard.Service.Statistics.StatisticsService where

import Wizard.Database.DAO.Branch.BranchDAO
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Database.DAO.User.UserDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Statistics.InstanceStatistics
import WizardLib.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import WizardLib.KnowledgeModel.Database.DAO.Package.PackageDAO

getInstanceStatistics :: AppContextM InstanceStatistics
getInstanceStatistics = do
  uCount <- countUsers
  pCount <- countPackagesGroupedByOrganizationIdAndKmId
  qCount <- countQuestionnaires
  bCount <- countBranches
  docCount <- countDocuments
  tmlCount <- countDocumentTemplatesGroupedByOrganizationIdAndKmId
  return
    InstanceStatistics
      { userCount = uCount
      , pkgCount = pCount
      , qtnCount = qCount
      , branchCount = bCount
      , docCount = docCount
      , tmlCount = tmlCount
      }
