module Wizard.Service.Organization.OrganizationMapper where

import Control.Lens ((^.))

import LensesConfig
import Shared.Api.Resource.Organization.OrganizationSimpleDTO
import Wizard.Integration.Resource.Organization.OrganizationSimpleIDTO

fromSimpleIntegration :: OrganizationSimpleIDTO -> OrganizationSimpleDTO
fromSimpleIntegration org =
  OrganizationSimpleDTO
    { _organizationSimpleDTOName = org ^. name
    , _organizationSimpleDTOOrganizationId = org ^. organizationId
    , _organizationSimpleDTOLogo = org ^. logo
    }
