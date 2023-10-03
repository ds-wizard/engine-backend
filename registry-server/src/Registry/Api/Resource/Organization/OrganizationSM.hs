module Registry.Api.Resource.Organization.OrganizationSM where

import Data.Swagger

import RegistryLib.Api.Resource.Organization.OrganizationDTO
import RegistryLib.Api.Resource.Organization.OrganizationJM ()
import RegistryLib.Database.Migration.Development.Organization.Data.Organizations
import RegistryLib.Model.Organization.OrganizationRole
import Shared.Common.Util.Swagger

instance ToSchema OrganizationRole

instance ToSchema OrganizationDTO where
  declareNamedSchema = toSwagger orgGlobalDTO
