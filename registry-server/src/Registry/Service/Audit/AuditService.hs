module Registry.Service.Audit.AuditService (
  auditListPackages,
  auditGetKnowledgeModelBundle,
  auditGetDocumentTemplateBundle,
  auditGetLocaleBundle,
) where

import Control.Monad.Reader (asks, liftIO)
import Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Time
import Text.Read (readMaybe)

import Registry.Database.DAO.Audit.AuditEntryDAO
import Registry.Model.Audit.AuditEntry
import Registry.Model.Context.AppContext
import Registry.Model.Statistics.InstanceStatistics
import RegistryLib.Model.Organization.Organization
import Shared.Common.Constant.Api
import Shared.Common.Model.Error.Error

auditListPackages :: [(String, String)] -> AppContextM (Either AppError (Maybe AuditEntry))
auditListPackages headers =
  heGetOrganizationFromContext $ \org -> do
    now <- liftIO getCurrentTime
    let iStat = getInstanceStaticsFromHeaders headers
    let entry =
          ListPackagesAuditEntry {organizationId = org.organizationId, instanceStatistics = iStat, createdAt = now}
    insertAuditEntry entry
    return . Right . Just $ entry

auditGetKnowledgeModelBundle :: String -> AppContextM (Either AppError (Maybe AuditEntry))
auditGetKnowledgeModelBundle pkgId =
  heGetOrganizationFromContext $ \org -> do
    now <- liftIO getCurrentTime
    let entry = GetKnowledgeModelBundleAuditEntry {organizationId = org.organizationId, knowledgeModelPackageId = pkgId, createdAt = now}
    insertAuditEntry entry
    return . Right . Just $ entry

auditGetDocumentTemplateBundle :: String -> AppContextM (Either AppError (Maybe AuditEntry))
auditGetDocumentTemplateBundle documentTemplateId =
  heGetOrganizationFromContext $ \org -> do
    now <- liftIO getCurrentTime
    let entry = GetDocumentTemplateBundleAuditEntry {organizationId = org.organizationId, documentTemplateId = documentTemplateId, createdAt = now}
    insertAuditEntry entry
    return . Right . Just $ entry

auditGetLocaleBundle :: String -> AppContextM (Either AppError (Maybe AuditEntry))
auditGetLocaleBundle localeId =
  heGetOrganizationFromContext $ \org -> do
    now <- liftIO getCurrentTime
    let entry = GetLocaleBundleAuditEntry {organizationId = org.organizationId, localeId = localeId, createdAt = now}
    insertAuditEntry entry
    return . Right . Just $ entry

-- --------------------------------
-- PRIVATE
-- --------------------------------
heGetOrganizationFromContext callback = do
  mOrg <- asks currentOrganization
  case mOrg of
    Just org -> callback org
    Nothing -> return . Right $ Nothing

-- -----------------------------------------------------
getInstanceStaticsFromHeaders headers =
  let get key = fromMaybe (-1) (M.lookup key (M.fromList headers) >>= readMaybe)
   in InstanceStatistics
        { userCount = get xUserCountHeaderName
        , pkgCount = get xKnowledgeModelPackageCountHeaderName
        , qtnCount = get xQtnCountHeaderName
        , kmEditorCount = get xKnowledgeModelEditorCountHeaderName
        , docCount = get xDocCountHeaderName
        , tmlCount = get xTmlCountHeaderName
        }
