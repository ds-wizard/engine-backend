module WizardLib.Common.Api.Resource.Organization.OrganizationSimpleDTO where

import GHC.Generics

data OrganizationSimpleDTO = OrganizationSimpleDTO
  { organizationId :: String
  , name :: String
  , logo :: Maybe String
  }
  deriving (Show, Eq, Generic)
