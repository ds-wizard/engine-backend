module Registry.Api.Resource.Organization.OrganizationStateSM where

import Data.Swagger

import Registry.Api.Resource.Organization.OrganizationStateDTO
import Registry.Api.Resource.Organization.OrganizationStateJM ()
import Registry.Database.Migration.Development.Organization.Data.Organizations
import Shared.Util.Swagger

instance ToSchema OrganizationStateDTO where
  declareNamedSchema = simpleToSchema orgStateDto
