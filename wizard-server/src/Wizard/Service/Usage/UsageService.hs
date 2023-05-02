module Wizard.Service.Usage.UsageService where

import qualified Data.UUID as U

import Shared.Locale.Database.DAO.Locale.LocaleDAO
import Wizard.Api.Resource.Usage.UsageDTO
import Wizard.Database.DAO.Branch.BranchDAO
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Database.DAO.DocumentTemplate.DocumentTemplateDraftDAO
import Wizard.Database.DAO.Limit.AppLimitDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Database.DAO.User.UserDAO
import Wizard.Model.Context.AppContext
import Wizard.Service.Usage.UsageMapper
import WizardLib.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateAssetDAO
import WizardLib.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import WizardLib.KnowledgeModel.Database.DAO.Package.PackageDAO

getUsage :: U.UUID -> AppContextM UsageDTO
getUsage appUuid = do
  appLimit <- findAppLimitByUuid appUuid
  userCount <- countUsersWithApp appUuid
  activeUserCount <- countActiveUsersWithApp appUuid
  branchCount <- countBranchesWithApp appUuid
  kmCount <- countPackagesGroupedByOrganizationIdAndKmIdWithApp appUuid
  qtnCount <- countQuestionnairesWithApp appUuid
  documentTemplateCount <- countDocumentTemplatesGroupedByOrganizationIdAndKmIdWithApp appUuid
  documentTemplateDraftCount <- countDraftsGroupedByOrganizationIdAndKmIdWithApp appUuid
  docCount <- countDocumentsWithApp appUuid
  localeCount <- countLocalesGroupedByOrganizationIdAndLocaleIdWithApp appUuid
  docSize <- sumDocumentFileSizeWithApp appUuid
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
