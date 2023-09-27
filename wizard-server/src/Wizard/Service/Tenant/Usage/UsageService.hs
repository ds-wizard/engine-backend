module Wizard.Service.Tenant.Usage.UsageService where

import qualified Data.UUID as U

import Shared.Locale.Database.DAO.Locale.LocaleDAO
import Wizard.Api.Resource.Tenant.Usage.TenantUsageDTO
import Wizard.Database.DAO.Branch.BranchDAO
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Database.DAO.DocumentTemplate.DocumentTemplateDraftDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Database.DAO.Tenant.TenantLimitBundleDAO
import Wizard.Database.DAO.User.UserDAO
import Wizard.Model.Context.AppContext
import Wizard.Service.Tenant.Usage.UsageMapper
import WizardLib.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateAssetDAO
import WizardLib.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import WizardLib.KnowledgeModel.Database.DAO.Package.PackageDAO

getUsage :: U.UUID -> AppContextM TenantUsageDTO
getUsage tenantUuid = do
  limitBundle <- findLimitBundleByUuid tenantUuid
  userCount <- countUsersWithTenant tenantUuid
  activeUserCount <- countActiveUsersWithTenant tenantUuid
  branchCount <- countBranchesWithTenant tenantUuid
  kmCount <- countPackagesGroupedByOrganizationIdAndKmIdWithTenant tenantUuid
  qtnCount <- countQuestionnairesWithTenant tenantUuid
  documentTemplateCount <- countDocumentTemplatesGroupedByOrganizationIdAndKmIdWithTenant tenantUuid
  documentTemplateDraftCount <- countDraftsGroupedByOrganizationIdAndKmIdWithTenant tenantUuid
  docCount <- countDocumentsWithTenant tenantUuid
  localeCount <- countLocalesGroupedByOrganizationIdAndLocaleIdWithTenant tenantUuid
  docSize <- sumDocumentFileSizeWithTenant tenantUuid
  templateAssetSize <- sumAssetFileSizeWithTenant tenantUuid
  let storageCount = docSize + templateAssetSize
  return $ toDTO limitBundle userCount activeUserCount branchCount kmCount qtnCount documentTemplateCount documentTemplateDraftCount docCount localeCount storageCount

getUsageForCurrentApp :: AppContextM TenantUsageDTO
getUsageForCurrentApp = do
  limitBundle <- findLimitBundleForCurrentTenant
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
  return $ toDTO limitBundle userCount activeUserCount branchCount kmCount qtnCount documentTemplateCount documentTemplateDraftCount docCount localeCount storageCount
