module Registry.Api.Resource.Organization.OrganizationCreateSM where

import Data.Swagger

import Registry.Api.Resource.Organization.OrganizationCreateDTO
import Registry.Api.Resource.Organization.OrganizationCreateJM ()
import Registry.Database.Migration.Development.Organization.Data.Organizations
import Shared.Util.Swagger

instance ToSchema OrganizationCreateDTO where
  declareNamedSchema = simpleToSchema orgGlobalCreate
