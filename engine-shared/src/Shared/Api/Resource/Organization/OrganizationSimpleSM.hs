module Shared.Api.Resource.Organization.OrganizationSimpleSM where

import Data.Swagger

import Shared.Api.Resource.Organization.OrganizationSimpleDTO
import Shared.Api.Resource.Organization.OrganizationSimpleJM ()
import Shared.Database.Migration.Development.Organization.Data.Organizations
import Shared.Util.Swagger

instance ToSchema OrganizationSimpleDTO where
  declareNamedSchema = simpleToSchema orgGlobalSimple
