module Api.Resource.Organization.Common where

import Data.Aeson

import Model.Organization.Organization

instance ToJSON OrganizationRole

instance FromJSON OrganizationRole
