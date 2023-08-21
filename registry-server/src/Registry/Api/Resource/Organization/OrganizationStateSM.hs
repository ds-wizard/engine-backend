module Registry.Api.Resource.Organization.OrganizationStateSM where

import Data.Swagger

import RegistryLib.Api.Resource.Organization.OrganizationStateDTO
import RegistryLib.Api.Resource.Organization.OrganizationStateJM ()
import RegistryLib.Database.Migration.Development.Organization.Data.Organizations
import Shared.Common.Util.Swagger

instance ToSchema OrganizationStateDTO where
  declareNamedSchema = toSwagger orgStateDto
