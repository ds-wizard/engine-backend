module Wizard.Api.Resource.Registry.RegistryOrganizationSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Registry.RegistryOrganizationJM ()
import Wizard.Database.Migration.Development.Registry.Data.RegistryOrganizations
import Wizard.Model.Registry.RegistryOrganization

instance ToSchema RegistryOrganization where
  declareNamedSchema = toSwagger globalRegistryOrganization
