module Wizard.Model.Config.ServerConfigDM where

import Shared.Model.Config.Environment
import Shared.Model.Config.ServerConfigDM
import Wizard.Model.Config.ServerConfig

defaultConfig :: ServerConfig
defaultConfig =
  ServerConfig
    { _serverConfigGeneral = defaultGeneral
    , _serverConfigDatabase = defaultDatabase
    , _serverConfigMessaging = defaultMessaging
    , _serverConfigJwt = defaultJwt
    , _serverConfigRoles = defaultRoles
    , _serverConfigMail = defaultMail
    , _serverConfigRegistry = defaultRegistry
    , _serverConfigAnalytics = defaultAnalytics
    , _serverConfigFeedback = defaultFeedback
    }

defaultGeneral :: ServerConfigGeneral
defaultGeneral =
  ServerConfigGeneral
    { _serverConfigGeneralEnvironment = Production
    , _serverConfigGeneralClientUrl = ""
    , _serverConfigGeneralServerPort = 3000
    , _serverConfigGeneralServiceToken = ""
    , _serverConfigGeneralSecret = ""
    , _serverConfigGeneralIntegrationConfig = "engine-wizard/config/integration.yml"
    , _serverConfigGeneralTemplateFolder = "engine-wizard/templates"
    , _serverConfigGeneralRemoteLocalizationUrl = Nothing
    , _serverConfigGeneralDebugLogHttpClient = True
    }

defaultMessaging :: ServerConfigMessaging
defaultMessaging =
  ServerConfigMessaging
    { _serverConfigMessagingEnabled = True
    , _serverConfigMessagingHost = "rabbitmq"
    , _serverConfigMessagingPort = 5672
    , _serverConfigMessagingUsername = "guest"
    , _serverConfigMessagingPassword = "guest"
    , _serverConfigMessagingVhost = "/"
    }

defaultJwt :: ServerConfigJwt
defaultJwt = ServerConfigJwt {_serverConfigJwtVersion = 1, _serverConfigJwtExpiration = 14}

defaultRoles :: ServerConfigRoles
defaultRoles =
  ServerConfigRoles
    { _serverConfigRolesAdmin =
        [ "UM_PERM"
        , "KM_PERM"
        , "KM_UPGRADE_PERM"
        , "KM_PUBLISH_PERM"
        , "PM_READ_PERM"
        , "PM_WRITE_PERM"
        , "QTN_PERM"
        , "DMP_PERM"
        , "CFG_PERM"
        , "SUBM_PERM"
        ]
    , _serverConfigRolesDataSteward =
        [ "KM_PERM"
        , "KM_UPGRADE_PERM"
        , "KM_PUBLISH_PERM"
        , "PM_READ_PERM"
        , "PM_WRITE_PERM"
        , "QTN_PERM"
        , "DMP_PERM"
        , "SUBM_PERM"
        ]
    , _serverConfigRolesResearcher = ["PM_READ_PERM", "QTN_PERM", "DMP_PERM", "SUBM_PERM"]
    }

defaultRegistry :: ServerConfigRegistry
defaultRegistry =
  ServerConfigRegistry
    { _serverConfigRegistryUrl = "https://api.registry.ds-wizard.org"
    , _serverConfigRegistryClientUrl = "https://registry.ds-wizard.org"
    }

defaultFeedback :: ServerConfigFeedback
defaultFeedback =
  ServerConfigFeedback
    {_serverConfigFeedbackApiUrl = "https://api.github.com", _serverConfigFeedbackWebUrl = "https://github.com"}
