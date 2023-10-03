module RegistryLib.Api.Resource.Organization.OrganizationSimpleSM where

import Data.Swagger

import RegistryLib.Api.Resource.Organization.OrganizationSimpleJM ()
import RegistryLib.Database.Migration.Development.Organization.Data.Organizations
import RegistryLib.Model.Organization.OrganizationSimple
import Shared.Common.Util.Swagger

instance ToSchema OrganizationSimple where
  declareNamedSchema = toSwagger orgGlobalSimple
