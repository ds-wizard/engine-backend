module WizardLib.Public.Model.Config.ServerConfigJM where

import Control.Monad (mzero)
import Data.Aeson

import WizardLib.Public.Model.Config.ServerConfig
import WizardLib.Public.Model.Config.ServerConfigDM

instance FromJSON ServerConfigExternalLink where
  parseJSON (Object o) = do
    allowedDomains <- o .:? "allowedDomains" .!= defaultExternalLink.allowedDomains
    return ServerConfigExternalLink {..}
  parseJSON _ = mzero
