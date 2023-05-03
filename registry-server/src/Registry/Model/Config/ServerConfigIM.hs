module Registry.Model.Config.ServerConfigIM where

import Registry.Model.Config.ServerConfig
import Shared.Common.Model.Config.ServerConfigIM

instance FromEnv ServerConfig where
  applyEnv serverConfig = do
    general <- applyEnv serverConfig.general
    database <- applyEnv serverConfig.database
    s3 <- applyEnv serverConfig.s3
    analytics <- applyEnv serverConfig.analytics
    sentry <- applyEnv serverConfig.sentry
    logging <- applyEnv serverConfig.logging
    cloud <- applyEnv serverConfig.cloud
    return ServerConfig {..}

instance FromEnv ServerConfigGeneral where
  applyEnv serverConfig =
    applyEnvVariables
      serverConfig
      [ \c -> applyEnvVariable "GENERAL_ENVIRONMENT" c.environment (\x -> c {environment = x})
      , \c -> applyStringEnvVariable "GENERAL_CLIENT_URL" c.clientUrl (\x -> c {clientUrl = x} :: ServerConfigGeneral)
      , \c -> applyEnvVariable "GENERAL_SERVER_PORT" c.serverPort (\x -> c {serverPort = x})
      ]
