module Registry.Api.Resource.Organization.OrganizationChangeSM where

import Data.Swagger

import Registry.Api.Resource.Organization.OrganizationChangeDTO
import Registry.Api.Resource.Organization.OrganizationChangeJM ()
import Registry.Database.Migration.Development.Organization.Data.Organizations
import Shared.Common.Util.Swagger

instance ToSchema OrganizationChangeDTO where
  declareNamedSchema = toSwagger orgGlobalEditedChange
