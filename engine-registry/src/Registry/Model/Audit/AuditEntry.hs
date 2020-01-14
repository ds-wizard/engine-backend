module Registry.Model.Audit.AuditEntry where

import Data.Time
import GHC.Generics

import Registry.Model.Statistics.InstanceStatistics

data AuditEntry
  = ListPackagesAuditEntry' ListPackagesAuditEntry
  | GetPackageBundleAuditEntry' GetPackageBundleAuditEntry
  deriving (Show, Eq, Generic)

data ListPackagesAuditEntry =
  ListPackagesAuditEntry
    { _listPackagesAuditEntryOrganizationId :: String
    , _listPackagesAuditEntryInstanceStatistics :: InstanceStatistics
    , _listPackagesAuditEntryCreatedAt :: UTCTime
    }
  deriving (Show, Generic)

instance Eq ListPackagesAuditEntry where
  ae1 == ae2 =
    _listPackagesAuditEntryOrganizationId ae1 == _listPackagesAuditEntryOrganizationId ae2 &&
    _listPackagesAuditEntryInstanceStatistics ae1 == _listPackagesAuditEntryInstanceStatistics ae2

data GetPackageBundleAuditEntry =
  GetPackageBundleAuditEntry
    { _getPackageBundleAuditEntryOrganizationId :: String
    , _getPackageBundleAuditEntryPackageId :: String
    , _getPackageBundleAuditEntryCreatedAt :: UTCTime
    }
  deriving (Show, Generic)

instance Eq GetPackageBundleAuditEntry where
  ae1 == ae2 =
    _getPackageBundleAuditEntryOrganizationId ae1 == _getPackageBundleAuditEntryOrganizationId ae2 &&
    _getPackageBundleAuditEntryPackageId ae1 == _getPackageBundleAuditEntryPackageId ae2
