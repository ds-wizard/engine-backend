module Wizard.Model.Config.AppConfigEM where

import Shared.Util.Crypto (encryptAES256WithB64)
import Wizard.Model.Common.SensitiveData
import Wizard.Model.Config.AppConfig

instance SensitiveData AppConfig where
  process key entity =
    entity
      { authentication = process key entity.authentication
      , registry = process key entity.registry
      , knowledgeModel = process key entity.knowledgeModel
      , questionnaire = process key entity.questionnaire
      }

instance SensitiveData AppConfigOrganization

instance SensitiveData AppConfigAuth where
  process key entity = entity {external = process key entity.external}

instance SensitiveData AppConfigAuthInternal

instance SensitiveData AppConfigAuthExternal where
  process key entity =
    entity {services = fmap (process key) entity.services}

instance SensitiveData AppConfigAuthExternalService where
  process key entity =
    entity
      { clientId = encryptAES256WithB64 key entity.clientId
      , clientSecret = encryptAES256WithB64 key entity.clientSecret
      }

instance SensitiveData AppConfigAuthExternalServiceParameter

instance SensitiveData AppConfigAuthExternalServiceStyle

instance SensitiveData AppConfigPrivacyAndSupport

instance SensitiveData AppConfigDashboard

instance SensitiveData AppConfigDashboardDashboardType

instance SensitiveData AppConfigLookAndFeel

instance SensitiveData AppConfigLookAndFeelCustomMenuLink

instance SensitiveData AppConfigRegistry where
  process key entity = entity {token = encryptAES256WithB64 key entity.token}

instance SensitiveData AppConfigKnowledgeModel where
  process key entity =
    entity {integrationConfig = encryptAES256WithB64 key entity.integrationConfig}

instance SensitiveData AppConfigQuestionnaire where
  process key entity = entity {feedback = process key entity.feedback}

instance SensitiveData AppConfigQuestionnaireFeedback where
  process key entity = entity {token = encryptAES256WithB64 key entity.token}
