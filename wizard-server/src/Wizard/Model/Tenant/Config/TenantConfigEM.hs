module Wizard.Model.Tenant.Config.TenantConfigEM where

import Shared.Common.Model.Common.SensitiveData
import Shared.Common.Util.Crypto (encryptAES256WithB64)
import Shared.OpenId.Model.OpenId.OpenIdClientParameter
import Shared.OpenId.Model.OpenId.OpenIdClientStyle
import Wizard.Model.Tenant.Config.TenantConfig
import WizardLib.Public.Model.Tenant.Config.TenantConfig
import WizardLib.Public.Model.Tenant.Config.TenantConfigEM ()

instance SensitiveData TenantConfigOrganization

instance SensitiveData TenantConfigAuthentication where
  process key entity = entity {external = process key entity.external}

instance SensitiveData TenantConfigAuthenticationInternal

instance SensitiveData TenantConfigAuthenticationExternal where
  process key entity =
    entity {services = fmap (process key) entity.services}

instance SensitiveData TenantConfigAuthenticationExternalService where
  process key entity =
    entity
      { clientId = encryptAES256WithB64 key entity.clientId
      , clientSecret = encryptAES256WithB64 key entity.clientSecret
      }

instance SensitiveData OpenIdClientParameter

instance SensitiveData OpenIdClientStyle

instance SensitiveData TenantConfigPrivacyAndSupport

instance SensitiveData TenantConfigDashboardAndLoginScreen

instance SensitiveData TenantConfigDashboardAndLoginScreenDashboardType

instance SensitiveData TenantConfigLookAndFeel

instance SensitiveData TenantConfigLookAndFeelCustomMenuLink

instance SensitiveData TenantConfigRegistry where
  process key entity = entity {token = encryptAES256WithB64 key entity.token}

instance SensitiveData TenantConfigKnowledgeModel where
  process key entity =
    entity {integrationConfig = encryptAES256WithB64 key entity.integrationConfig}

instance SensitiveData TenantConfigQuestionnaire where
  process key entity = entity {feedback = process key entity.feedback}

instance SensitiveData TenantConfigQuestionnaireFeedback where
  process key entity = entity {token = encryptAES256WithB64 key entity.token}

instance SensitiveData TenantConfigAiAssistant
