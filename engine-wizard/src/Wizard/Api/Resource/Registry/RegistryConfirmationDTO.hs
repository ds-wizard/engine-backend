module Wizard.Api.Resource.Registry.RegistryConfirmationDTO where

import GHC.Generics

data RegistryConfirmationDTO = RegistryConfirmationDTO
  { organizationId :: String
  , hash :: String
  }
  deriving (Show, Eq, Generic)
