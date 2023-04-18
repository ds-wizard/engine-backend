module Registry.Database.Migration.Development.Audit.Data.AuditEntries where

import Data.Maybe (fromJust)
import Data.Time

import Registry.Database.Migration.Development.Organization.Data.Organizations
import Registry.Database.Migration.Development.Statistics.Data.InstanceStatistics
import Registry.Model.Audit.AuditEntry
import Registry.Model.Organization.Organization
import WizardLib.KnowledgeModel.Database.Migration.Development.Package.Data.Packages
import WizardLib.KnowledgeModel.Model.Package.PackageWithEvents

listPackagesAuditEntry :: AuditEntry
listPackagesAuditEntry =
  ListPackagesAuditEntry
    { organizationId = orgGlobal.organizationId
    , instanceStatistics = iStat
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }

getPackageBundleAuditEntry :: AuditEntry
getPackageBundleAuditEntry =
  GetPackageBundleAuditEntry
    { organizationId = orgGlobal.organizationId
    , packageId = netherlandsPackageV2.pId
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }
