module Wizard.Service.Statistics.StatisticsService where

import Shared.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageDAO
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Database.DAO.KnowledgeModel.KnowledgeModelEditorDAO
import Wizard.Database.DAO.Project.ProjectDAO
import Wizard.Database.DAO.User.UserDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Statistics.InstanceStatistics

getInstanceStatistics :: AppContextM InstanceStatistics
getInstanceStatistics = do
  uCount <- countUsers
  pCount <- countPackagesGroupedByOrganizationIdAndKmId
  qCount <- countProjects
  bCount <- countKnowledgeModelEditors
  docCount <- countDocuments
  tmlCount <- countDocumentTemplatesGroupedByOrganizationIdAndKmId
  return
    InstanceStatistics
      { userCount = uCount
      , pkgCount = pCount
      , prjCount = qCount
      , knowledgeModelEditorCount = bCount
      , docCount = docCount
      , tmlCount = tmlCount
      }
