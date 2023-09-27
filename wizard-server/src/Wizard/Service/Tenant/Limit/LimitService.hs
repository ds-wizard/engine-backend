module Wizard.Service.Tenant.Limit.LimitService where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Data.Time
import qualified Data.UUID as U
import GHC.Int

import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Error.Error
import Shared.Locale.Database.DAO.Locale.LocaleDAO
import Wizard.Database.DAO.Branch.BranchDAO
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Database.DAO.DocumentTemplate.DocumentTemplateDraftDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Database.DAO.Tenant.TenantLimitBundleDAO
import Wizard.Database.DAO.User.UserDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Tenant.Limit.TenantLimitBundle
import Wizard.Service.Tenant.Limit.LimitMapper
import WizardLib.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateAssetDAO
import WizardLib.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import WizardLib.KnowledgeModel.Database.DAO.Package.PackageDAO

createLimitBundle :: U.UUID -> UTCTime -> AppContextM TenantLimitBundle
createLimitBundle tenantUuid now = do
  let limitBundle = fromCreate tenantUuid now
  insertLimitBundle limitBundle
  return limitBundle

recomputeLimitBundle :: U.UUID -> Maybe Int -> AppContextM TenantLimitBundle
recomputeLimitBundle tenantUuid mUsers = do
  limitBundle <- findLimitBundleByUuid tenantUuid
  let updatedTenantLimitBundle = fromChange limitBundle mUsers
  updateLimitBundleByUuid updatedTenantLimitBundle

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
  let storageCount = docSize + templateAssetSize
  checkLimit "storage" (storageCount + newFileSize) limit.storage

checkLimit :: (Show number, Ord number) => String -> number -> Maybe number -> AppContextM ()
checkLimit name count mMaxCount =
  case mMaxCount of
    Just maxCount ->
      when (count >= maxCount) (throwError . UserError $ _ERROR_SERVICE_TENANT__LIMIT_EXCEEDED name count maxCount)
    Nothing -> return ()
