module WizardLib.Public.Database.Mapping.OpenId.OpenIdClient where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Shared.OpenId.Api.Resource.OpenId.Client.Definition.OpenIdClientParameterJM ()
import Shared.OpenId.Api.Resource.OpenId.Client.Definition.OpenIdClientStyleJM ()
import WizardLib.Public.Model.OpenId.OpenIdClient

instance ToRow OpenIdClient where
  toRow OpenIdClient {..} =
    [ toField uuid
    , toField name
    , toField url
    , toField clientId
    , toField clientSecret
    , toJSONField parameters
    , toJSONField style
    , toField tenantUuid
    , toField createdAt
    , toField updatedAt
    , toField registrationEnabled
    , toField scopeProfile
    , toField scopeEmail
    ]

instance FromRow OpenIdClient where
  fromRow = do
    uuid <- field
    name <- field
    url <- field
    clientId <- field
    clientSecret <- field
    parameters <- fieldWith fromJSONField
    style <- fieldWith fromJSONField
    tenantUuid <- field
    createdAt <- field
    updatedAt <- field
    registrationEnabled <- field
    scopeProfile <- field
    scopeEmail <- field
    return $ OpenIdClient {..}
