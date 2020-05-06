module Wizard.Api.Resource.Registry.RegistryCreateDTO where

import GHC.Generics

data RegistryCreateDTO =
  RegistryCreateDTO
    { _registryCreateDTOEmail :: String
    }
  deriving (Show, Eq, Generic)
