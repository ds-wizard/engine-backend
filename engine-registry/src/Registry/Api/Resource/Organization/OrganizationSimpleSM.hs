module Registry.Api.Resource.Organization.OrganizationSimpleSM where

import Data.Swagger

import Registry.Api.Resource.Organization.OrganizationSimpleDTO
import Registry.Api.Resource.Organization.OrganizationSimpleJM ()
import Registry.Database.Migration.Development.Organization.Data.Organizations
import Registry.Service.Organization.OrganizationMapper
import Shared.Util.Swagger

instance ToSchema OrganizationSimpleDTO where
  declareNamedSchema = simpleToSchema "_organizationSimpleDTO" (toSimpleDTO orgGlobal)
