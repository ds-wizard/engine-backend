module Registry.Database.BSON.Audit.AuditEntry where

import qualified Data.Bson as BSON
import Data.Bson.Generic

import Registry.Database.BSON.Statistics.InstanceStatistics ()
import Registry.Model.Audit.AuditEntry

instance ToBSON AuditEntry where
  toBSON (ListPackagesAuditEntry' event) = toBSON event
  toBSON (GetPackageBundleAuditEntry' event) = toBSON event

instance FromBSON AuditEntry where
  fromBSON doc = do
    questionType <- BSON.lookup "type" doc
    case questionType of
      "ListPackagesAuditEntry" -> ListPackagesAuditEntry' <$> (fromBSON doc :: Maybe ListPackagesAuditEntry)
      "GetPackageBundleAuditEntry" -> GetPackageBundleAuditEntry' <$> (fromBSON doc :: Maybe GetPackageBundleAuditEntry)

instance ToBSON ListPackagesAuditEntry where
  toBSON ListPackagesAuditEntry {..} =
    [ "type" BSON.=: "ListPackagesAuditEntry"
    , "organizationId" BSON.=: _listPackagesAuditEntryOrganizationId
    , "instanceStatistics" BSON.=: _listPackagesAuditEntryInstanceStatistics
    , "createdAt" BSON.=: _listPackagesAuditEntryCreatedAt
    ]

instance FromBSON ListPackagesAuditEntry where
  fromBSON doc = do
    _listPackagesAuditEntryOrganizationId <- BSON.lookup "organizationId" doc
    _listPackagesAuditEntryInstanceStatistics <- BSON.lookup "instanceStatistics" doc
    _listPackagesAuditEntryCreatedAt <- BSON.lookup "createdAt" doc
    return ListPackagesAuditEntry {..}

instance ToBSON GetPackageBundleAuditEntry where
  toBSON GetPackageBundleAuditEntry {..} =
    [ "type" BSON.=: "GetPackageBundleAuditEntry"
    , "organizationId" BSON.=: _getPackageBundleAuditEntryOrganizationId
    , "packageId" BSON.=: _getPackageBundleAuditEntryPackageId
    , "createdAt" BSON.=: _getPackageBundleAuditEntryCreatedAt
    ]

instance FromBSON GetPackageBundleAuditEntry where
  fromBSON doc = do
    _getPackageBundleAuditEntryOrganizationId <- BSON.lookup "organizationId" doc
    _getPackageBundleAuditEntryPackageId <- BSON.lookup "packageId" doc
    _getPackageBundleAuditEntryCreatedAt <- BSON.lookup "createdAt" doc
    return GetPackageBundleAuditEntry {..}
