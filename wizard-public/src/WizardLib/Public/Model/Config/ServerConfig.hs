module WizardLib.Public.Model.Config.ServerConfig where

import GHC.Generics

data ServerConfigExternalLink = ServerConfigExternalLink
  { allowedDomains :: [String]
  }
  deriving (Generic, Show)
