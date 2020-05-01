module Wizard.Api.Resource.Registry.RegistryCreateSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Registry.RegistryCreateDTO
import Wizard.Api.Resource.Registry.RegistryCreateJM ()
import Wizard.Database.Migration.Development.Registry.Data.Registries

instance ToSchema RegistryCreateDTO where
  declareNamedSchema = simpleToSchema registryCreate
