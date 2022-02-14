module Registry.Model.Config.ServerConfigDM where

import Registry.Model.Config.ServerConfig
import Shared.Model.Config.Environment
import Shared.Model.Config.ServerConfigDM

defaultConfig :: ServerConfig
defaultConfig =
  ServerConfig
    { _serverConfigGeneral = defaultGeneral
    , _serverConfigDatabase = defaultDatabase
    , _serverConfigS3 = defaultS3
    , _serverConfigMail = defaultMail
    , _serverConfigAnalytics = defaultAnalytics
    , _serverConfigLogging = defaultLogging
    , _serverConfigCloud = defaultCloud
    }

defaultGeneral :: ServerConfigGeneral
defaultGeneral =
  ServerConfigGeneral
    { _serverConfigGeneralEnvironment = Production
    , _serverConfigGeneralClientUrl = ""
    , _serverConfigGeneralServerPort = 3000
    , _serverConfigGeneralTemplateFolder = "engine-registry/"
    , _serverConfigGeneralRemoteLocalizationUrl = Nothing
    }
