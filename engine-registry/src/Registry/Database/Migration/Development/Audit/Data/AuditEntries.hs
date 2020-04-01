module Registry.Database.Migration.Development.Audit.Data.AuditEntries where

import Control.Lens ((^.))
import Data.Maybe (fromJust)
import Data.Time

import LensesConfig
import Registry.Database.Migration.Development.Organization.Data.Organizations
import Registry.Database.Migration.Development.Statistics.Data.InstanceStatistics
import Registry.Model.Audit.AuditEntry
import Shared.Database.Migration.Development.Package.Data.Packages

listPackagesAuditEntry :: AuditEntry
listPackagesAuditEntry =
  ListPackagesAuditEntry' $
  ListPackagesAuditEntry
    { _listPackagesAuditEntryOrganizationId = orgGlobal ^. organizationId
    , _listPackagesAuditEntryInstanceStatistics = iStat
    , _listPackagesAuditEntryCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }

getPackageBundleAuditEntry :: AuditEntry
getPackageBundleAuditEntry =
  GetPackageBundleAuditEntry' $
  GetPackageBundleAuditEntry
    { _getPackageBundleAuditEntryOrganizationId = orgGlobal ^. organizationId
    , _getPackageBundleAuditEntryPackageId = netherlandsPackageV2 ^. pId
    , _getPackageBundleAuditEntryCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }
