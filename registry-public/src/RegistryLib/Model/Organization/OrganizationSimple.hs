module RegistryLib.Model.Organization.OrganizationSimple where

import GHC.Generics

data OrganizationSimple = OrganizationSimple
  { organizationId :: String
  , name :: String
  , logo :: Maybe String
  }
  deriving (Show, Eq, Generic)
