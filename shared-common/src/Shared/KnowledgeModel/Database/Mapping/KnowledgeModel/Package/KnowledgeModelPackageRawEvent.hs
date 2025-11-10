module Shared.KnowledgeModel.Database.Mapping.KnowledgeModel.Package.KnowledgeModelPackageRawEvent where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackageRawEvent

instance ToRow KnowledgeModelPackageRawEvent where
  toRow KnowledgeModelPackageRawEvent {..} =
    [ toField uuid
    , toField parentUuid
    , toField entityUuid
    , toJSONField content
    , toField packageId
    , toField tenantUuid
    , toField createdAt
    ]

instance FromRow KnowledgeModelPackageRawEvent where
  fromRow = do
    uuid <- field
    parentUuid <- field
    entityUuid <- field
    content <- fieldWith fromJSONField
    packageId <- field
    tenantUuid <- field
    createdAt <- field
    return $ KnowledgeModelPackageRawEvent {..}
