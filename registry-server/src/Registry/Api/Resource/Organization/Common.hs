module Registry.Api.Resource.Organization.Common where

import Data.Aeson

import Registry.Model.Organization.Organization

instance ToJSON OrganizationRole

instance FromJSON OrganizationRole
