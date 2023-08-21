module Registry.Model.Config.ServerConfigDM where

import Registry.Model.Config.ServerConfig
import Shared.Common.Model.Config.Environment
import Shared.Common.Model.Config.ServerConfigDM

defaultConfig :: ServerConfig
defaultConfig =
  ServerConfig
    { general = defaultGeneral
    , database = defaultDatabase
    , s3 = defaultS3
    , analytics = defaultAnalytics
    , sentry = defaultSentry
    , logging = defaultLogging
    , persistentCommand = defaultPersistentCommand
    , cloud = defaultCloud
    }

defaultGeneral :: ServerConfigGeneral
defaultGeneral =
  ServerConfigGeneral
    { environment = Production
    , clientUrl = ""
    , serverPort = 3000
    , publicRegistrationEnabled = True
    }
