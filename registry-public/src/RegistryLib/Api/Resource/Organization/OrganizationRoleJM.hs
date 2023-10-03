module RegistryLib.Api.Resource.Organization.OrganizationRoleJM where

import Data.Aeson

import RegistryLib.Model.Organization.OrganizationRole

instance ToJSON OrganizationRole

instance FromJSON OrganizationRole
