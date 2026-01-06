module Wizard.Service.Tenant.Usage.UsageService where

import qualified Data.UUID as U

import Shared.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateAssetDAO
import Shared.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Locale.Database.DAO.Locale.LocaleDAO
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Database.DAO.DocumentTemplate.DocumentTemplateDraftDAO
import Wizard.Database.DAO.KnowledgeModel.KnowledgeModelEditorDAO
import Wizard.Database.DAO.Project.ProjectDAO
import Wizard.Database.DAO.Project.ProjectFileDAO
import Wizard.Database.DAO.Tenant.TenantLimitBundleDAO
import Wizard.Database.DAO.User.UserDAO
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import Wizard.Service.Tenant.Usage.UsageMapper
import WizardLib.Public.Api.Resource.Tenant.Usage.WizardUsageDTO

getUsage :: U.UUID -> AppContextM WizardUsageDTO
getUsage tenantUuid = do
  limitBundle <- findLimitBundleByUuid tenantUuid
  userCount <- countUsersWithTenant tenantUuid
  activeUserCount <- countActiveUsersWithTenant tenantUuid
  knowledgeModelEditorCount <- countKnowledgeModelEditorsWithTenant tenantUuid
  kmCount <- countPackagesGroupedByOrganizationIdAndKmIdWithTenant tenantUuid
  prjCount <- countProjectsWithTenant tenantUuid
  documentTemplateCount <- countDocumentTemplatesGroupedByOrganizationIdAndKmIdWithTenant tenantUuid
  documentTemplateDraftCount <- countDraftsGroupedByOrganizationIdAndKmIdWithTenant tenantUuid
  docCount <- countDocumentsWithTenant tenantUuid
  localeCount <- countLocalesGroupedByOrganizationIdAndLocaleIdWithTenant tenantUuid
  docSize <- sumDocumentFileSizeWithTenant tenantUuid
  templateAssetSize <- sumAssetFileSizeWithTenant tenantUuid
  projectFileSize <- sumProjectFileSizeWithTenant tenantUuid
  let storageCount = docSize + templateAssetSize + projectFileSize
  return $ toDTO limitBundle userCount activeUserCount knowledgeModelEditorCount kmCount prjCount documentTemplateCount documentTemplateDraftCount docCount localeCount storageCount

getUsageForCurrentTenant :: AppContextM WizardUsageDTO
getUsageForCurrentTenant = do
  checkPermission _CFG_PERM
  limitBundle <- findLimitBundleForCurrentTenant
  userCount <- countUsers
  activeUserCount <- countActiveUsers
  knowledgeModelEditorCount <- countKnowledgeModelEditors
  kmCount <- countPackagesGroupedByOrganizationIdAndKmId
  prjCount <- countProjects
  documentTemplateCount <- countDocumentTemplatesGroupedByOrganizationIdAndKmId
  documentTemplateDraftCount <- countDraftsGroupedByOrganizationIdAndKmId
  docCount <- countDocuments
  localeCount <- countLocalesGroupedByOrganizationIdAndLocaleId
  docSize <- sumDocumentFileSize
  templateAssetSize <- sumAssetFileSize
  projectFileSize <- sumProjectFileSize
  let storageCount = docSize + templateAssetSize + projectFileSize
  return $ toDTO limitBundle userCount activeUserCount knowledgeModelEditorCount kmCount prjCount documentTemplateCount documentTemplateDraftCount docCount localeCount storageCount
