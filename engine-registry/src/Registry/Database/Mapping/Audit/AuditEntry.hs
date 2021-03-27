module Registry.Database.Mapping.Audit.AuditEntry where

import Data.Aeson
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Registry.Api.Resource.Statistics.InstanceStatisticsJM ()
import Registry.Model.Audit.AuditEntry
import Shared.Database.Mapping.Common

instance ToRow AuditEntry where
  toRow ListPackagesAuditEntry {..} =
    [ toStringField "ListPackagesAuditEntry"
    , toField _listPackagesAuditEntryOrganizationId
    , toJSONField _listPackagesAuditEntryInstanceStatistics
    , toStringField ""
    , toField _listPackagesAuditEntryCreatedAt
    ]
  toRow GetPackageBundleAuditEntry {..} =
    [ toStringField "GetPackageBundleAuditEntry"
    , toField _getPackageBundleAuditEntryOrganizationId
    , toStringField "{}"
    , toField _getPackageBundleAuditEntryPackageId
    , toField _getPackageBundleAuditEntryCreatedAt
    ]

instance FromRow AuditEntry where
  fromRow = do
    aType <- field
    case aType of
      "ListPackagesAuditEntry" -> do
        _listPackagesAuditEntryOrganizationId <- field
        _listPackagesAuditEntryInstanceStatistics <- fieldWith fromJSONField
        _ <- field :: RowParser String
        _listPackagesAuditEntryCreatedAt <- field
        return $ ListPackagesAuditEntry {..}
      "GetPackageBundleAuditEntry" -> do
        _getPackageBundleAuditEntryOrganizationId <- field
        _ <- fieldWith fromJSONField :: RowParser Object
        _getPackageBundleAuditEntryPackageId <- field
        _getPackageBundleAuditEntryCreatedAt <- field
        return $ GetPackageBundleAuditEntry {..}
