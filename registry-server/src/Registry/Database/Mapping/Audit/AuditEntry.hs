module Registry.Database.Mapping.Audit.AuditEntry where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Registry.Model.Audit.AuditEntry
import Registry.Model.Statistics.InstanceStatistics
import Shared.Common.Database.Mapping.Common

instance ToRow AuditEntry where
  toRow ListPackagesAuditEntry {..} =
    [ toStringField "ListPackagesAuditEntry"
    , toField organizationId
    , toField createdAt
    , toField instanceStatistics.userCount
    , toField instanceStatistics.pkgCount
    , toField instanceStatistics.branchCount
    , toField instanceStatistics.qtnCount
    , toField instanceStatistics.tmlCount
    , toField instanceStatistics.docCount
    , toField (Nothing :: Maybe String)
    , toField (Nothing :: Maybe String)
    , toField (Nothing :: Maybe String)
    ]
  toRow GetPackageBundleAuditEntry {..} =
    [ toStringField "GetPackageBundleAuditEntry"
    , toField organizationId
    , toField createdAt
    , toField (Nothing :: Maybe String)
    , toField (Nothing :: Maybe String)
    , toField (Nothing :: Maybe String)
    , toField (Nothing :: Maybe String)
    , toField (Nothing :: Maybe String)
    , toField (Nothing :: Maybe String)
    , toField packageId
    , toField (Nothing :: Maybe String)
    , toField (Nothing :: Maybe String)
    ]
  toRow GetDocumentTemplateBundleAuditEntry {..} =
    [ toStringField "GetDocumentTemplateBundleAuditEntry"
    , toField organizationId
    , toField createdAt
    , toField (Nothing :: Maybe String)
    , toField (Nothing :: Maybe String)
    , toField (Nothing :: Maybe String)
    , toField (Nothing :: Maybe String)
    , toField (Nothing :: Maybe String)
    , toField (Nothing :: Maybe String)
    , toField (Nothing :: Maybe String)
    , toField documentTemplateId
    , toField (Nothing :: Maybe String)
    ]
  toRow GetLocaleBundleAuditEntry {..} =
    [ toStringField "GetLocaleBundleAuditEntry"
    , toField organizationId
    , toField createdAt
    , toField (Nothing :: Maybe String)
    , toField (Nothing :: Maybe String)
    , toField (Nothing :: Maybe String)
    , toField (Nothing :: Maybe String)
    , toField (Nothing :: Maybe String)
    , toField (Nothing :: Maybe String)
    , toField (Nothing :: Maybe String)
    , toField (Nothing :: Maybe String)
    , toField localeId
    ]

instance FromRow AuditEntry where
  fromRow = do
    aType <- field
    case aType of
      "ListPackagesAuditEntry" -> do
        organizationId <- field
        createdAt <- field
        userCount <- field
        pkgCount <- field
        branchCount <- field
        qtnCount <- field
        tmlCount <- field
        docCount <- field
        let instanceStatistics = InstanceStatistics {..}
        _ <- field :: RowParser (Maybe String)
        _ <- field :: RowParser (Maybe String)
        _ <- field :: RowParser (Maybe String)
        return $ ListPackagesAuditEntry {..}
      "GetPackageBundleAuditEntry" -> do
        organizationId <- field
        createdAt <- field
        _ <- field :: RowParser (Maybe String)
        _ <- field :: RowParser (Maybe String)
        _ <- field :: RowParser (Maybe String)
        _ <- field :: RowParser (Maybe String)
        _ <- field :: RowParser (Maybe String)
        _ <- field :: RowParser (Maybe String)
        packageId <- field
        _ <- field :: RowParser (Maybe String)
        _ <- field :: RowParser (Maybe String)
        return $ GetPackageBundleAuditEntry {..}
      "GetDocumentTemplateBundleAuditEntry" -> do
        organizationId <- field
        createdAt <- field
        _ <- field :: RowParser (Maybe String)
        _ <- field :: RowParser (Maybe String)
        _ <- field :: RowParser (Maybe String)
        _ <- field :: RowParser (Maybe String)
        _ <- field :: RowParser (Maybe String)
        _ <- field :: RowParser (Maybe String)
        _ <- field :: RowParser (Maybe String)
        documentTemplateId <- field
        _ <- field :: RowParser (Maybe String)
        return $ GetDocumentTemplateBundleAuditEntry {..}
      "GetLocaleBundleAuditEntry" -> do
        organizationId <- field
        createdAt <- field
        _ <- field :: RowParser (Maybe String)
        _ <- field :: RowParser (Maybe String)
        _ <- field :: RowParser (Maybe String)
        _ <- field :: RowParser (Maybe String)
        _ <- field :: RowParser (Maybe String)
        _ <- field :: RowParser (Maybe String)
        _ <- field :: RowParser (Maybe String)
        _ <- field :: RowParser (Maybe String)
        localeId <- field
        return $ GetLocaleBundleAuditEntry {..}
      _ -> error $ "Unknown AuditEntry type: " ++ aType
