module Wizard.Service.Limit.AppLimitService where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Data.Time
import qualified Data.UUID as U
import GHC.Int

import Shared.Database.DAO.DocumentTemplate.DocumentTemplateAssetDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.Database.DAO.Locale.LocaleDAO
import Shared.Database.DAO.Package.PackageDAO
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Wizard.Database.DAO.Branch.BranchDAO
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Database.DAO.DocumentTemplate.DocumentTemplateDraftDAO
import Wizard.Database.DAO.Limit.AppLimitDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Database.DAO.User.UserDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Limit.AppLimit
import Wizard.Service.Limit.AppLimitMapper

getCurrentAppLimits :: AppContextM AppLimit
getCurrentAppLimits = findCurrentAppLimit

createAppLimit :: U.UUID -> UTCTime -> AppContextM AppLimit
createAppLimit aUuid now = do
  let appLimit = fromCreate aUuid now
  insertAppLimit appLimit
  return appLimit

recomputeAppLimit :: U.UUID -> Maybe Int -> AppContextM AppLimit
recomputeAppLimit appUuid mUsers = do
  appLimit <- findAppLimitById (U.toString appUuid)
  let updatedAppLimit = fromChange appLimit mUsers
  updateAppLimitById updatedAppLimit

checkUserLimit :: AppContextM ()
checkUserLimit = do
  limit <- findCurrentAppLimit
  count <- countUsers
  checkLimit "users" count limit.users

checkActiveUserLimit :: AppContextM ()
checkActiveUserLimit = do
  limit <- findCurrentAppLimit
  count <- countActiveUsers
  checkLimit "active users" count limit.activeUsers

checkBranchLimit :: AppContextM ()
checkBranchLimit = do
  limit <- findCurrentAppLimit
  count <- countBranches
  checkLimit "branches" count limit.branches

checkPackageLimit :: AppContextM ()
checkPackageLimit = do
  limit <- findCurrentAppLimit
  count <- countPackagesGroupedByOrganizationIdAndKmId
  checkLimit "knowledge models" count limit.knowledgeModels

checkQuestionnaireLimit :: AppContextM ()
checkQuestionnaireLimit = do
  limit <- findCurrentAppLimit
  count <- countQuestionnaires
  checkLimit "questionnaires" count limit.questionnaires

checkDocumentTemplateLimit :: AppContextM ()
checkDocumentTemplateLimit = do
  limit <- findCurrentAppLimit
  count <- countDocumentTemplatesGroupedByOrganizationIdAndKmId
  checkLimit "document template" count limit.documentTemplates

checkDocumentTemplateDraftLimit :: AppContextM ()
checkDocumentTemplateDraftLimit = do
  limit <- findCurrentAppLimit
  count <- countDraftsGroupedByOrganizationIdAndKmId
  checkLimit "document template draft" count limit.documentTemplateDrafts

checkDocumentLimit :: AppContextM ()
checkDocumentLimit = do
  limit <- findCurrentAppLimit
  count <- countDocuments
  checkLimit "documents" count limit.documents

checkLocaleLimit :: AppContextM ()
checkLocaleLimit = do
  limit <- findCurrentAppLimit
  count <- countLocalesGroupedByOrganizationIdAndLocaleId
  checkLimit "locales" count limit.locales

checkStorageSize :: Int64 -> AppContextM ()
checkStorageSize newFileSize = do
  limit <- findCurrentAppLimit
  docSize <- sumDocumentFileSize
  templateAssetSize <- sumAssetFileSize
  let storageCount = docSize + templateAssetSize
  checkLimit "storage" (storageCount + newFileSize) limit.storage

checkLimit :: (Show number, Ord number) => String -> number -> Maybe number -> AppContextM ()
checkLimit name count mMaxCount =
  case mMaxCount of
    Just maxCount ->
      when (count >= maxCount) (throwError . UserError $ _ERROR_SERVICE_APP__LIMIT_EXCEEDED name count maxCount)
    Nothing -> return ()
