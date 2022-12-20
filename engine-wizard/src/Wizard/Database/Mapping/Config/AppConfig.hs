module Wizard.Database.Mapping.Config.AppConfig where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Wizard.Api.Resource.Config.AppConfigJM ()
import Wizard.Model.Config.AppConfig

instance ToRow AppConfig where
  toRow AppConfig {..} =
    [ toField uuid
    , toJSONField organization
    , toJSONField authentication
    , toJSONField privacyAndSupport
    , toJSONField dashboard
    , toJSONField lookAndFeel
    , toJSONField registry
    , toJSONField knowledgeModel
    , toJSONField questionnaire
    , toJSONField template
    , toJSONField submission
    , toField createdAt
    , toField updatedAt
    , toJSONField feature
    , toJSONField owl
    , toField mailConfigUuid
    ]

instance FromRow AppConfig where
  fromRow = do
    uuid <- field
    organization <- fieldWith fromJSONField
    authentication <- fieldWith fromJSONField
    privacyAndSupport <- fieldWith fromJSONField
    dashboard <- fieldWith fromJSONField
    lookAndFeel <- fieldWith fromJSONField
    registry <- fieldWith fromJSONField
    knowledgeModel <- fieldWith fromJSONField
    questionnaire <- fieldWith fromJSONField
    template <- fieldWith fromJSONField
    submission <- fieldWith fromJSONField
    createdAt <- field
    updatedAt <- field
    feature <- fieldWith fromJSONField
    owl <- fieldWith fromJSONField
    mailConfigUuid <- field
    return $ AppConfig {..}
