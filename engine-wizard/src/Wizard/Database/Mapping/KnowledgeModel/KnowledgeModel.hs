module Wizard.Database.Mapping.KnowledgeModel.KnowledgeModel where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Shared.Api.Resource.KnowledgeModel.KnowledgeModelJM ()
import Shared.Model.KnowledgeModel.KnowledgeModel

instance ToRow Metric where
  toRow Metric {..} =
    [ toField _metricUuid
    , toField _metricTitle
    , toField _metricAbbreviation
    , toField _metricDescription
    , toJSONField _metricReferences
    , toField _metricCreatedAt
    , toField _metricUpdatedAt
    ]

instance FromRow Metric where
  fromRow = do
    _metricUuid <- field
    _metricTitle <- field
    _metricAbbreviation <- field
    _metricDescription <- field
    _metricReferences <- fieldWith fromJSONField
    _metricCreatedAt <- field
    _metricUpdatedAt <- field
    return $ Metric {..}
