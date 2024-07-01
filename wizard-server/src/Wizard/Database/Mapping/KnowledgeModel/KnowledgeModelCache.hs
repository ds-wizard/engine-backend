module Wizard.Database.Mapping.KnowledgeModel.KnowledgeModelCache where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types

import Shared.Common.Util.Uuid
import Wizard.Model.KnowledgeModel.KnowledgeModelCache
import WizardLib.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelJM ()

instance ToRow KnowledgeModelCache where
  toRow KnowledgeModelCache {..} =
    [ toField packageId
    , toField . PGArray $ tagUuids
    , toJSONField knowledgeModel
    , toField tenantUuid
    , toField createdAt
    ]

instance FromRow KnowledgeModelCache where
  fromRow = do
    packageId <- field
    tagUuidsS <- fromPGArray <$> field
    let tagUuids = fmap u' tagUuidsS
    knowledgeModel <- fieldWith fromJSONField
    tenantUuid <- field
    createdAt <- field
    return $ KnowledgeModelCache {..}
