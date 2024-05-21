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
    , sentry = defaultSentry
    , analytics = defaultAnalytics
    , logging = defaultLogging
    , cloud = defaultCloud
    , persistentCommand = defaultPersistentCommand
    }

defaultGeneral :: ServerConfigGeneral
defaultGeneral =
  ServerConfigGeneral
    { environment = Production
    , clientUrl = ""
    , serverPort = 3000
    , publicRegistrationEnabled = True
    }
