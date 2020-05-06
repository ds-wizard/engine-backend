module Wizard.Database.Migration.Development.Registry.Data.Registries where

import Registry.Database.Migration.Development.ActionKey.Data.ActionKeys
import Registry.Model.ActionKey.ActionKey
import Wizard.Api.Resource.Registry.RegistryConfirmationDTO
import Wizard.Api.Resource.Registry.RegistryCreateDTO

registryCreate :: RegistryCreateDTO
registryCreate = RegistryCreateDTO {_registryCreateDTOEmail = "albert.einstein@example.com"}

registryConfirmation :: RegistryConfirmationDTO
registryConfirmation =
  RegistryConfirmationDTO
    { _registryConfirmationDTOOrganizationId = _actionKeyOrganizationId regActionKey
    , _registryConfirmationDTOHash = _actionKeyHash regActionKey
    }
