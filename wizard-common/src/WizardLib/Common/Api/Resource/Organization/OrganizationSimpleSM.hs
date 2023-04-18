module WizardLib.Common.Api.Resource.Organization.OrganizationSimpleSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import WizardLib.Common.Api.Resource.Organization.OrganizationSimpleDTO
import WizardLib.Common.Api.Resource.Organization.OrganizationSimpleJM ()
import WizardLib.Common.Database.Migration.Development.Organization.Data.Organizations

instance ToSchema OrganizationSimpleDTO where
  declareNamedSchema = toSwagger orgGlobalSimple
