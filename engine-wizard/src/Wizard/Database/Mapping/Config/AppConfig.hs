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
    [ toField _appConfigAId
    , toJSONField _appConfigOrganization
    , toJSONField _appConfigAuthentication
    , toJSONField _appConfigPrivacyAndSupport
    , toJSONField _appConfigDashboard
    , toJSONField _appConfigLookAndFeel
    , toJSONField _appConfigRegistry
    , toJSONField _appConfigQuestionnaire
    , toJSONField _appConfigTemplate
    , toJSONField _appConfigSubmission
    , toField _appConfigCreatedAt
    , toField _appConfigUpdatedAt
    ]

instance FromRow AppConfig where
  fromRow = do
    _appConfigAId <- field
    _appConfigOrganization <- fieldWith fromJSONField
    _appConfigAuthentication <- fieldWith fromJSONField
    _appConfigPrivacyAndSupport <- fieldWith fromJSONField
    _appConfigDashboard <- fieldWith fromJSONField
    _appConfigLookAndFeel <- fieldWith fromJSONField
    _appConfigRegistry <- fieldWith fromJSONField
    _appConfigQuestionnaire <- fieldWith fromJSONField
    _appConfigTemplate <- fieldWith fromJSONField
    _appConfigSubmission <- fieldWith fromJSONField
    _appConfigCreatedAt <- field
    _appConfigUpdatedAt <- field
    return $ AppConfig {..}
