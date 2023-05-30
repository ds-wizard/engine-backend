module Wizard.Database.Migration.Development.Registry.Data.Registries where

import Registry.Database.Migration.Development.ActionKey.Data.ActionKeys
import Shared.ActionKey.Model.ActionKey.ActionKey
import Wizard.Api.Resource.Registry.RegistryConfirmationDTO
import Wizard.Api.Resource.Registry.RegistryCreateDTO

registryCreate :: RegistryCreateDTO
registryCreate = RegistryCreateDTO {email = "albert.einstein@example.com"}

registryConfirmation :: RegistryConfirmationDTO
registryConfirmation =
  RegistryConfirmationDTO
    { organizationId = regActionKey.identity
    , hash = regActionKey.hash
    }
