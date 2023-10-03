module Wizard.Database.Migration.Development.Registry.Data.Registries where

import Wizard.Api.Resource.Registry.RegistryConfirmationDTO
import Wizard.Api.Resource.Registry.RegistryCreateDTO

registryCreate :: RegistryCreateDTO
registryCreate = RegistryCreateDTO {email = "albert.einstein@example.com"}

registryConfirmation :: RegistryConfirmationDTO
registryConfirmation =
  RegistryConfirmationDTO
    { organizationId = "org.nl"
    , hash = "someHash"
    }
