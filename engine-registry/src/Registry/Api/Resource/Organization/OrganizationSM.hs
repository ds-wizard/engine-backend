module Registry.Api.Resource.Organization.OrganizationSM where

import Data.Swagger

import Registry.Api.Resource.Organization.OrganizationDTO
import Registry.Api.Resource.Organization.OrganizationJM ()
import Registry.Database.Migration.Development.Organization.Data.Organizations
import Registry.Model.Organization.Organization
import Shared.Util.Swagger

instance ToSchema OrganizationRole

instance ToSchema OrganizationDTO where
  declareNamedSchema = simpleToSchema orgGlobalDTO
