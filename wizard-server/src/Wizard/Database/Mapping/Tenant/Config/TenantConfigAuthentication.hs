module Wizard.Database.Mapping.Tenant.Config.TenantConfigAuthentication where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Shared.Common.Model.Config.SimpleFeature
import Shared.OpenId.Api.Resource.OpenId.Client.Definition.OpenIdClientParameterJM ()
import Shared.OpenId.Model.OpenId.OpenIdClientStyle
import Wizard.Model.Tenant.Config.TenantConfig

instance ToRow TenantConfigAuthentication where
  toRow TenantConfigAuthentication {..} =
    [ toField tenantUuid
    , toField defaultRole
    , toField internal.registration.enabled
    , toField internal.twoFactorAuth.enabled
    , toField internal.twoFactorAuth.codeLength
    , toField internal.twoFactorAuth.expiration
    , toField createdAt
    , toField updatedAt
    ]

instance FromRow TenantConfigAuthentication where
  fromRow = do
    tenantUuid <- field
    defaultRole <- field
    internalRegistrationEnabled <- field
    internalTwoFactorAuthEnabled <- field
    internalTwoFactorAuthCodeLength <- field
    internalTwoFactorAuthExpiration <- field
    let internal = TenantConfigAuthenticationInternal {registration = SimpleFeature {enabled = internalRegistrationEnabled}, twoFactorAuth = TenantConfigAuthenticationInternalTwoFactorAuth {enabled = internalTwoFactorAuthEnabled, codeLength = internalTwoFactorAuthCodeLength, expiration = internalTwoFactorAuthExpiration}}
    let external = TenantConfigAuthenticationExternal {services = []}
    createdAt <- field
    updatedAt <- field
    return $ TenantConfigAuthentication {..}

instance FromRow TenantConfigAuthenticationExternalService where
  fromRow = do
    aId <- field
    name <- field
    url <- field
    clientId <- field
    clientSecret <- field
    parameters <- fieldWith fromJSONField
    styleIcon <- field
    styleBackground <- field
    styleColor <- field
    let style = OpenIdClientStyle {icon = styleIcon, background = styleBackground, color = styleColor}
    tenantUuid <- field
    createdAt <- field
    updatedAt <- field
    return $ TenantConfigAuthenticationExternalService {..}

instance ToRow TenantConfigAuthenticationExternalService where
  toRow TenantConfigAuthenticationExternalService {..} =
    [ toField aId
    , toField name
    , toField url
    , toField clientId
    , toField clientSecret
    , toJSONField parameters
    , toField style.icon
    , toField style.background
    , toField style.color
    , toField tenantUuid
    , toField createdAt
    , toField updatedAt
    ]
