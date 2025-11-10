module Shared.KnowledgeModel.Database.Mapping.KnowledgeModel.Package.KnowledgeModelPackageEvent where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventJM ()
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackageEvent

instance ToRow KnowledgeModelPackageEvent where
  toRow KnowledgeModelPackageEvent {..} =
    [ toField uuid
    , toField parentUuid
    , toField entityUuid
    , toJSONField content
    , toField packageId
    , toField tenantUuid
    , toField createdAt
    ]

instance FromRow KnowledgeModelPackageEvent where
  fromRow = do
    uuid <- field
    parentUuid <- field
    entityUuid <- field
    content <- fieldWith fromJSONField
    packageId <- field
    tenantUuid <- field
    createdAt <- field
    return $ KnowledgeModelPackageEvent {..}
