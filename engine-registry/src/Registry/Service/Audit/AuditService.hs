module Registry.Service.Audit.AuditService
  ( auditListPackages
  , auditGetPackageBundle
  ) where

import Control.Lens ((^.))
import Control.Monad.Reader (asks, liftIO)
import Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Time
import Text.Read (readMaybe)

import LensesConfig
import Registry.Database.DAO.Audit.AuditEntryDAO
import Registry.Model.Audit.AuditEntry
import Registry.Model.Context.AppContext
import Registry.Model.Statistics.InstanceStatistics
import Shared.Constant.Api
import Shared.Model.Error.Error

auditListPackages :: [(String, String)] -> AppContextM (Either AppError (Maybe AuditEntry))
auditListPackages headers =
  heGetOrganizationFromContext $ \org -> do
    now <- liftIO getCurrentTime
    let iStat = getInstanceStaticsFromHeaders headers
    let entry =
          ListPackagesAuditEntry
            { _listPackagesAuditEntryOrganizationId = org ^. organizationId
            , _listPackagesAuditEntryInstanceStatistics = iStat
            , _listPackagesAuditEntryCreatedAt = now
            }
    insertAuditEntry entry
    return . Right . Just $ entry

auditGetPackageBundle :: String -> AppContextM (Either AppError (Maybe AuditEntry))
auditGetPackageBundle pkgId =
  heGetOrganizationFromContext $ \org -> do
    now <- liftIO getCurrentTime
    let entry =
          GetPackageBundleAuditEntry
            { _getPackageBundleAuditEntryOrganizationId = org ^. organizationId
            , _getPackageBundleAuditEntryPackageId = pkgId
            , _getPackageBundleAuditEntryCreatedAt = now
            }
    insertAuditEntry entry
    return . Right . Just $ entry

-- --------------------------------
-- PRIVATE
-- --------------------------------
heGetOrganizationFromContext callback = do
  mOrg <- asks _appContextCurrentOrganization
  case mOrg of
    Just org -> callback org
    Nothing -> return . Right $ Nothing

-- -----------------------------------------------------
getInstanceStaticsFromHeaders headers =
  let get key = fromMaybe (-1) (M.lookup key (M.fromList headers) >>= readMaybe)
   in InstanceStatistics
        { _instanceStatisticsUserCount = get xUserCountHeaderName
        , _instanceStatisticsPkgCount = get xPkgCountHeaderName
        , _instanceStatisticsQtnCount = get xQtnCountHeaderName
        }
