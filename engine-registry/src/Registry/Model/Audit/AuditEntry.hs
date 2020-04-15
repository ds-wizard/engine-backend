module Registry.Model.Audit.AuditEntry where

import Data.Time
import GHC.Generics

import Registry.Model.Statistics.InstanceStatistics

data AuditEntry
  = ListPackagesAuditEntry
      { _listPackagesAuditEntryOrganizationId :: String
      , _listPackagesAuditEntryInstanceStatistics :: InstanceStatistics
      , _listPackagesAuditEntryCreatedAt :: UTCTime
      }
  | GetPackageBundleAuditEntry
      { _getPackageBundleAuditEntryOrganizationId :: String
      , _getPackageBundleAuditEntryPackageId :: String
      , _getPackageBundleAuditEntryCreatedAt :: UTCTime
      }
  deriving (Show, Generic)

instance Eq AuditEntry where
  ae1@ListPackagesAuditEntry {} == ae2@ListPackagesAuditEntry {} =
    _listPackagesAuditEntryOrganizationId ae1 == _listPackagesAuditEntryOrganizationId ae2 &&
    _listPackagesAuditEntryInstanceStatistics ae1 == _listPackagesAuditEntryInstanceStatistics ae2
  ae1@GetPackageBundleAuditEntry {} == ae2@GetPackageBundleAuditEntry {} =
    _getPackageBundleAuditEntryOrganizationId ae1 == _getPackageBundleAuditEntryOrganizationId ae2 &&
    _getPackageBundleAuditEntryPackageId ae1 == _getPackageBundleAuditEntryPackageId ae2
