module Wizard.Api.Resource.Registry.RegistryConfirmationDTO where

import GHC.Generics

data RegistryConfirmationDTO =
  RegistryConfirmationDTO
    { _registryConfirmationDTOOrganizationId :: String
    , _registryConfirmationDTOHash :: String
    }
  deriving (Show, Eq, Generic)
