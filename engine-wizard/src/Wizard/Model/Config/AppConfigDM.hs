module Wizard.Model.Config.AppConfigDM where

import Shared.Model.Config.Environment
import Wizard.Model.Config.AppConfig

defaultGeneral :: AppConfigGeneral
defaultGeneral =
  AppConfigGeneral
    { _appConfigGeneralEnvironment = Production
    , _appConfigGeneralClientUrl = ""
    , _appConfigGeneralServerPort = 3000
    , _appConfigGeneralServiceToken = ""
    , _appConfigGeneralIntegrationConfig = "engine-wizard/config/integration.yml"
    , _appConfigGeneralTemplateFolder = "engine-wizard/templates"
    , _appConfigGeneralRemoteLocalizationUrl = Nothing
    , _appConfigGeneralRegistrationEnabled = True
    , _appConfigGeneralPublicQuestionnaireEnabled = False
    , _appConfigGeneralLevelsEnabled = True
    , _appConfigGeneralQuestionnaireAccessibilityEnabled = True
    }

defaultClient :: AppConfigClient
defaultClient =
  AppConfigClient
    { _appConfigClientPrivacyUrl = "https://ds-wizard.org/privacy.html"
    , _appConfigClientAppTitle = Nothing
    , _appConfigClientAppTitleShort = Nothing
    , _appConfigClientWelcomeWarning = Nothing
    , _appConfigClientWelcomeInfo = Nothing
    , _appConfigClientSupportEmail = Nothing
    , _appConfigClientSupportRepositoryName = Nothing
    , _appConfigClientSupportRepositoryUrl = Nothing
    , _appConfigClientDashboard = Just defaultClientDashboard
    , _appConfigClientCustomMenuLinks = []
    }

defaultClientDashboard :: AppConfigClientDashboard
defaultClientDashboard =
  AppConfigClientDashboard
    { _appConfigClientDashboardAdmin = ["Welcome"]
    , _appConfigClientDashboardDataSteward = ["Welcome"]
    , _appConfigClientDashboardResearcher = ["Welcome"]
    }

defaultDatabase :: AppConfigDatabase
defaultDatabase =
  AppConfigDatabase
    { _appConfigDatabaseHost = "mongo"
    , _appConfigDatabaseDatabaseName = "wizard-server"
    , _appConfigDatabasePort = 27017
    , _appConfigDatabaseAuthEnabled = False
    , _appConfigDatabaseUsername = ""
    , _appConfigDatabasePassword = ""
    }

defaultMessaging :: AppConfigMessaging
defaultMessaging =
  AppConfigMessaging
    { _appConfigMessagingEnabled = True
    , _appConfigMessagingHost = "rabbitmq"
    , _appConfigMessagingPort = 5672
    , _appConfigMessagingUsername = "guest"
    , _appConfigMessagingPassword = "guest"
    , _appConfigMessagingVhost = "/"
    }

defaultJwt :: AppConfigJwt
defaultJwt = AppConfigJwt {_appConfigJwtSecret = "", _appConfigJwtVersion = 1, _appConfigJwtExpiration = 14}

defaultRoles :: AppConfigRoles
defaultRoles =
  AppConfigRoles
    { _appConfigRolesDefaultRole = "DATASTEWARD"
    , _appConfigRolesAdmin =
        [ "UM_PERM"
        , "ORG_PERM"
        , "KM_PERM"
        , "KM_UPGRADE_PERM"
        , "KM_PUBLISH_PERM"
        , "PM_READ_PERM"
        , "PM_WRITE_PERM"
        , "QTN_PERM"
        , "DMP_PERM"
        ]
    , _appConfigRolesDataSteward =
        ["KM_PERM", "KM_UPGRADE_PERM", "KM_PUBLISH_PERM", "PM_READ_PERM", "PM_WRITE_PERM", "QTN_PERM", "DMP_PERM"]
    , _appConfigRolesResearcher = ["PM_READ_PERM", "QTN_PERM", "DMP_PERM"]
    }

defaultMail :: AppConfigMail
defaultMail =
  AppConfigMail
    { _appConfigMailEnabled = True
    , _appConfigMailName = "DS Wizard"
    , _appConfigMailEmail = ""
    , _appConfigMailHost = ""
    , _appConfigMailPort = 465
    , _appConfigMailSsl = False
    , _appConfigMailAuthEnabled = False
    , _appConfigMailUsername = ""
    , _appConfigMailPassword = ""
    }

defaultRegistry :: AppConfigRegistry
defaultRegistry =
  AppConfigRegistry
    { _appConfigRegistryEnabled = False
    , _appConfigRegistryUrl = "https://api.registry.ds-wizard.org"
    , _appConfigRegistryToken = ""
    , _appConfigRegistryClientUrl = "https://registry.ds-wizard.org"
    }

defaultAnalytics :: AppConfigAnalytics
defaultAnalytics = AppConfigAnalytics {_appConfigAnalyticsEnabled = False, _appConfigAnalyticsEmail = ""}

defaultFeedback :: AppConfigFeedback
defaultFeedback =
  AppConfigFeedback
    { _appConfigFeedbackEnabled = True
    , _appConfigFeedbackToken = ""
    , _appConfigFeedbackOwner = ""
    , _appConfigFeedbackRepo = ""
    , _appConfigFeedbackIssueUrl = "https://github.com/:owner/:repo/issues/:issueId"
    }
