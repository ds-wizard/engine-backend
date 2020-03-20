module Wizard.Api.Resource.Organization.OrganizationChangeSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Organization.OrganizationChangeDTO
import Wizard.Api.Resource.Organization.OrganizationChangeJM ()
import Wizard.Database.Migration.Development.Organization.Data.Organizations

instance ToSchema OrganizationChangeDTO where
  declareNamedSchema = simpleToSchema "_organizationChangeDTO" editedOrg1Change
