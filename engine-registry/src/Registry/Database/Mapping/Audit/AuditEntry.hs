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
    , toField organizationId
    , toJSONField instanceStatistics
    , toStringField ""
    , toField createdAt
    ]
  toRow GetPackageBundleAuditEntry {..} =
    [ toStringField "GetPackageBundleAuditEntry"
    , toField organizationId
    , toStringField "{}"
    , toField packageId
    , toField createdAt
    ]
  toRow GetDocumentTemplateBundleAuditEntry {..} =
    [ toStringField "GetDocumentTemplateBundleAuditEntry"
    , toField organizationId
    , toStringField "{}"
    , toField documentTemplateId
    , toField createdAt
    ]
  toRow GetLocaleBundleAuditEntry {..} =
    [ toStringField "GetLocaleBundleAuditEntry"
    , toField organizationId
    , toStringField "{}"
    , toField localeId
    , toField createdAt
    ]

instance FromRow AuditEntry where
  fromRow = do
    aType <- field
    case aType of
      "ListPackagesAuditEntry" -> do
        organizationId <- field
        instanceStatistics <- fieldWith fromJSONField
        _ <- field :: RowParser String
        createdAt <- field
        return $ ListPackagesAuditEntry {..}
      "GetPackageBundleAuditEntry" -> do
        organizationId <- field
        _ <- fieldWith fromJSONField :: RowParser Object
        packageId <- field
        createdAt <- field
        return $ GetPackageBundleAuditEntry {..}
      "GetDocumentTemplateBundleAuditEntry" -> do
        organizationId <- field
        _ <- fieldWith fromJSONField :: RowParser Object
        documentTemplateId <- field
        createdAt <- field
        return $ GetDocumentTemplateBundleAuditEntry {..}
      "GetLocaleBundleAuditEntry" -> do
        organizationId <- field
        _ <- fieldWith fromJSONField :: RowParser Object
        localeId <- field
        createdAt <- field
        return $ GetLocaleBundleAuditEntry {..}
