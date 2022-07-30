module Wizard.Service.Statistics.StatisticsService where

import Shared.Database.DAO.Package.PackageDAO
import Shared.Database.DAO.Template.TemplateDAO
import Wizard.Database.DAO.Branch.BranchDAO
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Database.DAO.User.UserDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Statistics.InstanceStatistics

getInstanceStatistics :: AppContextM InstanceStatistics
getInstanceStatistics = do
  uCount <- countUsers
  pCount <- countPackagesGroupedByOrganizationIdAndKmId
  qCount <- countQuestionnaires
  bCount <- countBranches
  docCount <- countDocuments
  tmlCount <- countTemplatesGroupedByOrganizationIdAndKmId
  return
    InstanceStatistics
      { _instanceStatisticsUserCount = uCount
      , _instanceStatisticsPkgCount = pCount
      , _instanceStatisticsQtnCount = qCount
      , _instanceStatisticsBranchCount = bCount
      , _instanceStatisticsDocCount = docCount
      , _instanceStatisticsTmlCount = tmlCount
      }
