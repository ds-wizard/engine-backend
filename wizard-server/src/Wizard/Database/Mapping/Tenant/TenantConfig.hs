module Wizard.Database.Mapping.Tenant.TenantConfig where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Wizard.Api.Resource.Tenant.Config.TenantConfigJM ()
import Wizard.Model.Tenant.Config.TenantConfig

instance ToRow TenantConfig where
  toRow TenantConfig {..} =
    [ toField uuid
    , toJSONField organization
    , toJSONField authentication
    , toJSONField privacyAndSupport
    , toJSONField dashboardAndLoginScreen
    , toJSONField lookAndFeel
    , toJSONField registry
    , toJSONField knowledgeModel
    , toJSONField questionnaire
    , toJSONField submission
    , toField createdAt
    , toField updatedAt
    , toJSONField owl
    , toField mailConfigUuid
    , toJSONField aiAssistant
    ]

instance FromRow TenantConfig where
  fromRow = do
    uuid <- field
    organization <- fieldWith fromJSONField
    authentication <- fieldWith fromJSONField
    privacyAndSupport <- fieldWith fromJSONField
    dashboardAndLoginScreen <- fieldWith fromJSONField
    lookAndFeel <- fieldWith fromJSONField
    registry <- fieldWith fromJSONField
    knowledgeModel <- fieldWith fromJSONField
    questionnaire <- fieldWith fromJSONField
    submission <- fieldWith fromJSONField
    createdAt <- field
    updatedAt <- field
    owl <- fieldWith fromJSONField
    mailConfigUuid <- field
    aiAssistant <- fieldWith fromJSONField
    return $ TenantConfig {..}
