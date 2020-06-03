module Shared.Model.Config.ServerConfigDM where

import Shared.Model.Config.ServerConfig

defaultDatabase :: ServerConfigDatabase
defaultDatabase =
  ServerConfigDatabase
    { _serverConfigDatabaseHost = "mongo"
    , _serverConfigDatabaseDatabaseName = "wizard-server"
    , _serverConfigDatabasePort = 27017
    , _serverConfigDatabaseAuthEnabled = False
    , _serverConfigDatabaseUsername = ""
    , _serverConfigDatabasePassword = ""
    }

defaultMail :: ServerConfigMail
defaultMail =
  ServerConfigMail
    { _serverConfigMailEnabled = True
    , _serverConfigMailName = "DS Wizard"
    , _serverConfigMailEmail = ""
    , _serverConfigMailHost = ""
    , _serverConfigMailPort = 465
    , _serverConfigMailSsl = False
    , _serverConfigMailAuthEnabled = False
    , _serverConfigMailUsername = ""
    , _serverConfigMailPassword = ""
    }

defaultAnalytics :: ServerConfigAnalytics
defaultAnalytics = ServerConfigAnalytics {_serverConfigAnalyticsEnabled = False, _serverConfigAnalyticsEmail = ""}
