module Wizard.Model.Registry.RegistryTemplate where

import Data.Time
import GHC.Generics

data RegistryTemplate =
  RegistryTemplate
    { _registryTemplateOrganizationId :: String
    , _registryTemplateTemplateId :: String
    , _registryTemplateRemoteVersion :: String
    , _registryTemplateCreatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)
