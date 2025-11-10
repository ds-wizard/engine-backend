module Registry.Database.Migration.Development.Audit.Data.AuditEntries where

import Data.Maybe (fromJust)
import Data.Time

import Registry.Database.Migration.Development.Statistics.Data.InstanceStatistics
import Registry.Model.Audit.AuditEntry
import RegistryLib.Database.Migration.Development.Organization.Data.Organizations
import RegistryLib.Model.Organization.Organization
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage

listPackagesAuditEntry :: AuditEntry
listPackagesAuditEntry =
  ListPackagesAuditEntry
    { organizationId = orgGlobal.organizationId
    , instanceStatistics = iStat
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }

getKnowledgeModelBundleAuditEntry :: AuditEntry
getKnowledgeModelBundleAuditEntry =
  GetKnowledgeModelBundleAuditEntry
    { organizationId = orgGlobal.organizationId
    , knowledgeModelPackageId = netherlandsKmPackageV2.pId
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }
