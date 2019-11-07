module Service.Audit.AuditService
  ( auditListPackages
  , auditGetPackageBundle
  -- Helpers
  , heAuditListPackages
  , heAuditGetPackageBundle
  ) where

import Control.Lens ((^.))
import Control.Monad.Reader (asks, liftIO)
import Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Time
import Text.Read (readMaybe)

import Constant.Api
import Database.DAO.Audit.AuditEntryDAO
import LensesConfig
import Model.Audit.AuditEntry
import Model.Context.AppContext
import Model.Error.Error
import Model.Statistics.InstanceStatistics
import Util.Helper (createHeeHelper)

auditListPackages :: [(String, String)] -> AppContextM (Either AppError (Maybe AuditEntry))
auditListPackages headers =
  heGetOrganizationFromContext $ \org -> do
    now <- liftIO getCurrentTime
    let iStat = getInstanceStaticsFromHeaders headers
    let entry =
          ListPackagesAuditEntry' $
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
          GetPackageBundleAuditEntry' $
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
     { _instanceStatisticsUserCount = get xDswUserCountHeaderName
     , _instanceStatisticsPkgCount = get xDswPkgCountHeaderName
     , _instanceStatisticsQtnCount = get xDswQtnCountHeaderName
     }

-- --------------------------------
-- HELPERS
-- --------------------------------
heAuditListPackages headers callback = createHeeHelper (auditListPackages headers) callback

-- -----------------------------------------------------
heAuditGetPackageBundle pkgId callback = createHeeHelper (auditGetPackageBundle pkgId) callback
