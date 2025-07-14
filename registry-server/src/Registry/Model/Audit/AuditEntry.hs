module Registry.Model.Audit.AuditEntry where

import Data.Time
import GHC.Generics

import Registry.Model.Statistics.InstanceStatistics

data AuditEntry
  = ListPackagesAuditEntry
      { organizationId :: String
      , instanceStatistics :: InstanceStatistics
      , createdAt :: UTCTime
      }
  | GetPackageBundleAuditEntry
      { organizationId :: String
      , packageId :: String
      , createdAt :: UTCTime
      }
  | GetDocumentTemplateBundleAuditEntry
      { organizationId :: String
      , documentTemplateId :: String
      , createdAt :: UTCTime
      }
  | GetLocaleBundleAuditEntry
      { organizationId :: String
      , localeId :: String
      , createdAt :: UTCTime
      }
  deriving (Show, Generic)

instance Eq AuditEntry where
  ae1@ListPackagesAuditEntry {} == ae2@ListPackagesAuditEntry {} =
    ae1.organizationId == ae2.organizationId
      && ae1.instanceStatistics == ae2.instanceStatistics
  ae1@GetPackageBundleAuditEntry {} == ae2@GetPackageBundleAuditEntry {} =
    ae1.organizationId == ae2.organizationId
      && ae1.packageId == ae2.packageId
  ae1@GetDocumentTemplateBundleAuditEntry {} == ae2@GetDocumentTemplateBundleAuditEntry {} =
    ae1.organizationId == ae2.organizationId
      && ae1.documentTemplateId == ae2.documentTemplateId
  ae1@GetLocaleBundleAuditEntry {} == ae2@GetLocaleBundleAuditEntry {} =
    ae1.organizationId == ae2.organizationId
      && ae1.localeId == ae2.localeId
  _ == _ = False
