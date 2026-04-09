module WizardLib.Public.Model.Config.ServerConfigIM where

import Shared.Common.Model.Config.ServerConfigIM

import WizardLib.Public.Model.Config.ServerConfig

instance FromEnv ServerConfigExternalLink where
  applyEnv serverConfig =
    applyEnvVariables
      serverConfig
      [ \c -> applyEnvVariable "EXTERNAL_LINK_ALLOWED_DOMAINS" c.allowedDomains (\x -> c {allowedDomains = x})
      ]
