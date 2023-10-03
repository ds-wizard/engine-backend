module Registry.Api.Resource.Organization.OrganizationCreateSM where

import Data.Swagger

import RegistryLib.Api.Resource.Organization.OrganizationCreateDTO
import RegistryLib.Api.Resource.Organization.OrganizationCreateJM ()
import RegistryLib.Database.Migration.Development.Organization.Data.Organizations
import Shared.Common.Util.Swagger

instance ToSchema OrganizationCreateDTO where
  declareNamedSchema = toSwagger orgGlobalCreate
