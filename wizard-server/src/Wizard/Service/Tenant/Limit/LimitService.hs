module Wizard.Service.Tenant.Limit.LimitService where

import Control.Monad.Reader (liftIO)
import Data.Time
import qualified Data.UUID as U
import GHC.Int

import Shared.Common.Service.Tenant.Limit.LimitService
import Shared.Locale.Database.DAO.Locale.LocaleDAO
import Wizard.Database.DAO.Branch.BranchDAO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Database.DAO.DocumentTemplate.DocumentTemplateDraftDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireFileDAO
import Wizard.Database.DAO.Tenant.TenantLimitBundleDAO
import Wizard.Database.DAO.User.UserDAO
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import Wizard.Model.Tenant.Limit.TenantLimitBundle
import Wizard.Service.Tenant.Limit.LimitMapper
import Wizard.Service.Tenant.Usage.UsageService
import WizardLib.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateAssetDAO
import WizardLib.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import WizardLib.KnowledgeModel.Database.DAO.Package.PackageDAO
import WizardLib.Public.Api.Resource.Tenant.Usage.WizardUsageDTO
import WizardLib.Public.Model.Tenant.Limit.TenantLimitBundleChange

createLimitBundle :: U.UUID -> UTCTime -> AppContextM TenantLimitBundle
createLimitBundle tenantUuid now = do
  let limitBundle = fromCreate tenantUuid now
  insertLimitBundle limitBundle
  return limitBundle

modifyLimitBundle :: U.UUID -> TenantLimitBundleChange -> AppContextM WizardUsageDTO
modifyLimitBundle tenantUuid reqDto =
  runInTransaction $ do
    checkPermission _TENANT_PERM
    limitBundle <- findLimitBundleByUuid tenantUuid
    now <- liftIO getCurrentTime
    let limitBundleUpdated = fromChangeDTO limitBundle reqDto now
    updateLimitBundleByUuid limitBundleUpdated
    getUsage tenantUuid

checkUserLimit :: AppContextM ()
checkUserLimit = do
  limit <- findLimitBundleForCurrentTenant
  count <- countUsers
  checkLimit "users" count limit.users

checkActiveUserLimit :: AppContextM ()
checkActiveUserLimit = do
  limit <- findLimitBundleForCurrentTenant
  count <- countActiveUsers
  checkLimit "active users" count limit.activeUsers

checkBranchLimit :: AppContextM ()
checkBranchLimit = do
  limit <- findLimitBundleForCurrentTenant
  count <- countBranches
  checkLimit "branches" count limit.branches

checkPackageLimit :: AppContextM ()
checkPackageLimit = do
  limit <- findLimitBundleForCurrentTenant
  count <- countPackagesGroupedByOrganizationIdAndKmId
  checkLimit "knowledge models" count limit.knowledgeModels

checkQuestionnaireLimit :: AppContextM ()
checkQuestionnaireLimit = do
  limit <- findLimitBundleForCurrentTenant
  count <- countQuestionnaires
  checkLimit "questionnaires" count limit.questionnaires

checkDocumentTemplateLimit :: AppContextM ()
checkDocumentTemplateLimit = do
  limit <- findLimitBundleForCurrentTenant
  count <- countDocumentTemplatesGroupedByOrganizationIdAndKmId
  checkLimit "document templates" count limit.documentTemplates

checkDocumentTemplateDraftLimit :: AppContextM ()
checkDocumentTemplateDraftLimit = do
  limit <- findLimitBundleForCurrentTenant
  count <- countDraftsGroupedByOrganizationIdAndKmId
  checkLimit "document template drafts" count limit.documentTemplateDrafts

checkDocumentLimit :: AppContextM ()
checkDocumentLimit = do
  limit <- findLimitBundleForCurrentTenant
  count <- countDocuments
  checkLimit "documents" count limit.documents

checkLocaleLimit :: AppContextM ()
checkLocaleLimit = do
  limit <- findLimitBundleForCurrentTenant
  count <- countLocalesGroupedByOrganizationIdAndLocaleId
  checkLimit "locales" count limit.locales

checkStorageSize :: Int64 -> AppContextM ()
checkStorageSize newFileSize = do
  limit <- findLimitBundleForCurrentTenant
  docSize <- sumDocumentFileSize
  templateAssetSize <- sumAssetFileSize
  qtnFileSize <- sumQuestionnaireFileSize
  let storageCount = docSize + templateAssetSize + qtnFileSize
  checkLimit "storage" (storageCount + newFileSize) limit.storage
