module Registry.Service.Audit.AuditService (
  auditListPackages,
  auditGetPackageBundle,
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
import Registry.Model.Organization.Organization
import Registry.Model.Statistics.InstanceStatistics
import Shared.Constant.Api
import Shared.Model.Error.Error

auditListPackages :: [(String, String)] -> AppContextM (Either AppError (Maybe AuditEntry))
auditListPackages headers =
  heGetOrganizationFromContext $ \org -> do
    now <- liftIO getCurrentTime
    let iStat = getInstanceStaticsFromHeaders headers
    let entry =
          ListPackagesAuditEntry {organizationId = org.organizationId, instanceStatistics = iStat, createdAt = now}
    insertAuditEntry entry
    return . Right . Just $ entry

auditGetPackageBundle :: String -> AppContextM (Either AppError (Maybe AuditEntry))
auditGetPackageBundle pkgId =
  heGetOrganizationFromContext $ \org -> do
    now <- liftIO getCurrentTime
    let entry = GetPackageBundleAuditEntry {organizationId = org.organizationId, packageId = pkgId, createdAt = now}
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
        , pkgCount = get xPkgCountHeaderName
        , qtnCount = get xQtnCountHeaderName
        , branchCount = get xBranchCountHeaderName
        , docCount = get xDocCountHeaderName
        , tmlCount = get xTmlCountHeaderName
        }
