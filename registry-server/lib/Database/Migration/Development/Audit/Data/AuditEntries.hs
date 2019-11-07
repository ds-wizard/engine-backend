module Database.Migration.Development.Audit.Data.AuditEntries where

import Control.Lens ((^.))
import Data.Maybe (fromJust)
import Data.Time

import Database.Migration.Development.Organization.Data.Organizations
import Database.Migration.Development.Package.Data.Packages
import Database.Migration.Development.Statistics.Data.InstanceStatistics
import LensesConfig
import Model.Audit.AuditEntry

listPackagesAuditEntry :: AuditEntry
listPackagesAuditEntry =
  ListPackagesAuditEntry' $
  ListPackagesAuditEntry
  { _listPackagesAuditEntryOrganizationId = orgDsw ^. organizationId
  , _listPackagesAuditEntryInstanceStatistics = iStat
  , _listPackagesAuditEntryCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
  }

getPackageBundleAuditEntry :: AuditEntry
getPackageBundleAuditEntry =
  GetPackageBundleAuditEntry' $
  GetPackageBundleAuditEntry
  { _getPackageBundleAuditEntryOrganizationId = orgDsw ^. organizationId
  , _getPackageBundleAuditEntryPackageId = netherlandsPackageV2 ^. pId
  , _getPackageBundleAuditEntryCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
  }
