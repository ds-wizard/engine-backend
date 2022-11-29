module Registry.Api.Resource.Organization.OrganizationDTO where

import Data.Time
import GHC.Generics

import Registry.Model.Organization.Organization

data OrganizationDTO = OrganizationDTO
  { organizationId :: String
  , name :: String
  , description :: String
  , email :: String
  , oRole :: OrganizationRole
  , token :: String
  , active :: Bool
  , logo :: Maybe String
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Generic)

instance Eq OrganizationDTO where
  a == b =
    a.organizationId == b.organizationId
      && a.name == b.name
      && a.description == b.description
      && a.email == b.email
      && a.oRole == b.oRole
      && a.token == b.token
      && a.active == b.active
      && a.logo == b.logo
