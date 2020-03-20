module Wizard.Api.Resource.Organization.OrganizationSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Organization.OrganizationDTO
import Wizard.Api.Resource.Organization.OrganizationJM ()
import Wizard.Database.Migration.Development.Organization.Data.Organizations
import Wizard.Service.Organization.OrganizationMapper

instance ToSchema OrganizationDTO where
  declareNamedSchema = simpleToSchema "_organizationDTO" (toDTO org1)
