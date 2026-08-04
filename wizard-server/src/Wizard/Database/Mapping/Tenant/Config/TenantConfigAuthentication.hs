module Wizard.Database.Mapping.Tenant.Config.TenantConfigAuthentication where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Shared.Common.Model.Config.SimpleFeature
import Wizard.Model.Tenant.Config.TenantConfig

instance ToRow TenantConfigAuthentication where
  toRow TenantConfigAuthentication {..} =
    [ toField tenantUuid
    , toField defaultRoleUuid
    , toField internal.registration.enabled
    , toField internal.twoFactorAuth.enabled
    , toField internal.twoFactorAuth.codeLength
    , toField internal.twoFactorAuth.expiration
    , toField createdAt
    , toField updatedAt
    , toField internal.nonAdminLoginEnabled
    , toField internal.sessionExpiration
    , toField internal.userEmailLinkExpiration
    ]

instance FromRow TenantConfigAuthentication where
  fromRow = do
    tenantUuid <- field
    defaultRoleUuid <- field
    internalRegistrationEnabled <- field
    internalTwoFactorAuthEnabled <- field
    internalTwoFactorAuthCodeLength <- field
    internalTwoFactorAuthExpiration <- field
    createdAt <- field
    updatedAt <- field
    internalNonAdminLoginEnabled <- field
    internalSessionExpiration <- field
    internalUserEmailLinkExpiration <- field
    let internal = TenantConfigAuthenticationInternal {registration = SimpleFeature {enabled = internalRegistrationEnabled}, nonAdminLoginEnabled = internalNonAdminLoginEnabled, sessionExpiration = internalSessionExpiration, userEmailLinkExpiration = internalUserEmailLinkExpiration, twoFactorAuth = TenantConfigAuthenticationInternalTwoFactorAuth {enabled = internalTwoFactorAuthEnabled, codeLength = internalTwoFactorAuthCodeLength, expiration = internalTwoFactorAuthExpiration}}
    return $ TenantConfigAuthentication {..}
