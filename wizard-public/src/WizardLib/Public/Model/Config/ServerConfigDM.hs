module WizardLib.Public.Model.Config.ServerConfigDM where

import WizardLib.Public.Model.Config.ServerConfig

defaultExternalLink :: ServerConfigExternalLink
defaultExternalLink =
  ServerConfigExternalLink
    { allowedDomains = ["ds-wizard.org"]
    }
