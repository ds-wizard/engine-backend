module Wizard.Api.Resource.Organization.OrganizationSimpleSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Organization.OrganizationSimpleDTO
import Wizard.Api.Resource.Organization.OrganizationSimpleJM ()
import Wizard.Database.Migration.Development.Organization.Data.Organizations

instance ToSchema OrganizationSimpleDTO where
  declareNamedSchema = simpleToSchema "_organizationSimpleDTO" orgGlobal
