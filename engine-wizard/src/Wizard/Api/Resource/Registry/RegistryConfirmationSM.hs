module Wizard.Api.Resource.Registry.RegistryConfirmationSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Registry.RegistryConfirmationDTO
import Wizard.Api.Resource.Registry.RegistryConfirmationJM ()
import Wizard.Database.Migration.Development.Registry.Data.Registries

instance ToSchema RegistryConfirmationDTO where
  declareNamedSchema = simpleToSchema registryConfirmation
