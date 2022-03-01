module Wizard.Service.Usage.UsageService where

import Shared.Database.DAO.Package.PackageDAO
import Shared.Database.DAO.Template.TemplateAssetDAO
import Shared.Database.DAO.Template.TemplateDAO
import Wizard.Api.Resource.Usage.UsageDTO
import Wizard.Database.DAO.App.AppLimitDAO
import Wizard.Database.DAO.Branch.BranchDAO
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Database.DAO.User.UserDAO
import Wizard.Model.Context.AppContext
import Wizard.Service.Usage.UsageMapper

getUsageForCurrentApp :: AppContextM UsageDTO
getUsageForCurrentApp = do
  appLimit <- findCurrentAppLimit
  userCount <- countUsers
  activeUserCount <- countActiveUsers
  branchCount <- countBranches
  kmCount <- countPackagesGroupedByOrganizationIdAndKmId
  qtnCount <- countQuestionnaires
  tmlCount <- countTemplatesGroupedByOrganizationIdAndKmId
  docCount <- countDocuments
  docSize <- sumDocumentFileSize
  templateAssetSize <- sumTemplateAssetFileSize
  let storageCount = docSize + templateAssetSize
  return $ toDTO appLimit userCount activeUserCount branchCount kmCount qtnCount tmlCount docCount storageCount
