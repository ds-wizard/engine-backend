module Wizard.Model.Tenant.Config.TenantConfigEM where

import Shared.Common.Model.Common.SensitiveData
import Shared.Common.Util.Crypto (encryptAES256WithB64)
import Wizard.Model.Tenant.Config.TenantConfig
import WizardLib.Public.Model.Tenant.Config.TenantConfig
import WizardLib.Public.Model.Tenant.Config.TenantConfigEM ()

instance SensitiveData TenantConfigOrganization

instance SensitiveData TenantConfigAuthentication

instance SensitiveData TenantConfigAuthenticationInternal

instance SensitiveData TenantConfigPrivacyAndSupport

instance SensitiveData TenantConfigDashboardAndLoginScreen

instance SensitiveData TenantConfigDashboardAndLoginScreenDashboardType

instance SensitiveData TenantConfigLookAndFeel

instance SensitiveData TenantConfigLookAndFeelCustomMenuLink

instance SensitiveData TenantConfigRegistry where
  process key entity = entity {token = encryptAES256WithB64 key entity.token}

instance SensitiveData TenantConfigProject where
  process key entity = entity {feedback = process key entity.feedback}

instance SensitiveData TenantConfigProjectFeedback where
  process key entity = entity {token = encryptAES256WithB64 key entity.token}

instance SensitiveData TenantConfigFeatures
