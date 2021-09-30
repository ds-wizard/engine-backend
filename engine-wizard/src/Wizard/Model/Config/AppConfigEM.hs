module Wizard.Model.Config.AppConfigEM where

import Shared.Util.Crypto (encryptAES256WithB64)
import Wizard.Model.Common.SensitiveData
import Wizard.Model.Config.AppConfig

instance SensitiveData AppConfig where
  process key entity =
    entity
      { _appConfigAuthentication = process key (_appConfigAuthentication entity)
      , _appConfigRegistry = process key (_appConfigRegistry entity)
      , _appConfigQuestionnaire = process key (_appConfigQuestionnaire entity)
      }

instance SensitiveData AppConfigOrganization

instance SensitiveData AppConfigAuth where
  process key entity = entity {_appConfigAuthExternal = process key (_appConfigAuthExternal entity)}

instance SensitiveData AppConfigAuthInternal

instance SensitiveData AppConfigAuthExternal where
  process key entity =
    entity {_appConfigAuthExternalServices = fmap (process key) (_appConfigAuthExternalServices entity)}

instance SensitiveData AppConfigAuthExternalService where
  process key entity =
    entity
      { _appConfigAuthExternalServiceClientId = encryptAES256WithB64 key (_appConfigAuthExternalServiceClientId entity)
      , _appConfigAuthExternalServiceClientSecret =
          encryptAES256WithB64 key (_appConfigAuthExternalServiceClientSecret entity)
      }

instance SensitiveData AppConfigAuthExternalServiceParameter

instance SensitiveData AppConfigAuthExternalServiceStyle

instance SensitiveData AppConfigPrivacyAndSupport

instance SensitiveData AppConfigDashboard

instance SensitiveData AppConfigDashboardWidgets

instance SensitiveData AppConfigLookAndFeel

instance SensitiveData AppConfigLookAndFeelCustomMenuLink

instance SensitiveData AppConfigRegistry where
  process key entity = entity {_appConfigRegistryToken = encryptAES256WithB64 key (_appConfigRegistryToken entity)}

instance SensitiveData AppConfigQuestionnaire where
  process key entity = entity {_appConfigQuestionnaireFeedback = process key (_appConfigQuestionnaireFeedback entity)}

instance SensitiveData AppConfigQuestionnaireFeedback where
  process key entity =
    entity
      {_appConfigQuestionnaireFeedbackToken = encryptAES256WithB64 key (_appConfigQuestionnaireFeedbackToken entity)}
