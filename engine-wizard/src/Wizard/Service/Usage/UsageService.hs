module Wizard.Service.Usage.UsageService where

import Shared.Database.DAO.DocumentTemplate.DocumentTemplateAssetDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.Database.DAO.Locale.LocaleDAO
import Shared.Database.DAO.Package.PackageDAO
import Shared.Util.Uuid
import Wizard.Api.Resource.Usage.UsageDTO
import Wizard.Database.DAO.Branch.BranchDAO
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Database.DAO.DocumentTemplate.DocumentTemplateDraftDAO
import Wizard.Database.DAO.Limit.AppLimitDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Database.DAO.User.UserDAO
import Wizard.Model.Context.AppContext
import Wizard.Service.Usage.UsageMapper

getUsage :: String -> AppContextM UsageDTO
getUsage appUuid = do
  appLimit <- findAppLimitById appUuid
  userCount <- countUsersWithApp appUuid
  activeUserCount <- countActiveUsersWithApp appUuid
  branchCount <- countBranchesWithApp appUuid
  kmCount <- countPackagesGroupedByOrganizationIdAndKmIdWithApp appUuid
  qtnCount <- countQuestionnairesWithApp appUuid
  documentTemplateCount <- countDocumentTemplatesGroupedByOrganizationIdAndKmIdWithApp (u' appUuid)
  documentTemplateDraftCount <- countDraftsGroupedByOrganizationIdAndKmIdWithApp (u' appUuid)
  docCount <- countDocumentsWithApp (u' appUuid)
  localeCount <- countLocalesGroupedByOrganizationIdAndLocaleIdWithApp (u' appUuid)
  docSize <- sumDocumentFileSizeWithApp (u' appUuid)
  templateAssetSize <- sumAssetFileSizeWithApp appUuid
  let storageCount = docSize + templateAssetSize
  return $ toDTO appLimit userCount activeUserCount branchCount kmCount qtnCount documentTemplateCount documentTemplateDraftCount docCount localeCount storageCount

getUsageForCurrentApp :: AppContextM UsageDTO
getUsageForCurrentApp = do
  appLimit <- findCurrentAppLimit
  userCount <- countUsers
  activeUserCount <- countActiveUsers
  branchCount <- countBranches
  kmCount <- countPackagesGroupedByOrganizationIdAndKmId
  qtnCount <- countQuestionnaires
  documentTemplateCount <- countDocumentTemplatesGroupedByOrganizationIdAndKmId
  documentTemplateDraftCount <- countDraftsGroupedByOrganizationIdAndKmId
  docCount <- countDocuments
  localeCount <- countLocalesGroupedByOrganizationIdAndLocaleId
  docSize <- sumDocumentFileSize
  templateAssetSize <- sumAssetFileSize
  let storageCount = docSize + templateAssetSize
  return $ toDTO appLimit userCount activeUserCount branchCount kmCount qtnCount documentTemplateCount documentTemplateDraftCount docCount localeCount storageCount
