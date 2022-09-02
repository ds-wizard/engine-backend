module Wizard.Model.Registry.RegistryOrganization where

import Data.Time
import GHC.Generics

data RegistryOrganization =
  RegistryOrganization
    { _registryOrganizationOrganizationId :: String
    , _registryOrganizationName :: String
    , _registryOrganizationLogo :: Maybe String
    , _registryOrganizationCreatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)
