module Api.Resource.Organization.OrganizationDTO where

import Data.Time
import GHC.Generics

import Model.Organization.Organization

data OrganizationDTO = OrganizationDTO
  { _organizationDTOOrganizationId :: String
  , _organizationDTOName :: String
  , _organizationDTODescription :: String
  , _organizationDTOEmail :: String
  , _organizationDTORole :: OrganizationRole
  , _organizationDTOToken :: String
  , _organizationDTOActive :: Bool
  , _organizationDTOLogo :: Maybe String
  , _organizationDTOCreatedAt :: UTCTime
  , _organizationDTOUpdatedAt :: UTCTime
  } deriving (Show, Generic)

instance Eq OrganizationDTO where
  a == b =
    _organizationDTOOrganizationId a == _organizationDTOOrganizationId b &&
    _organizationDTOName a == _organizationDTOName b &&
    _organizationDTODescription a == _organizationDTODescription b &&
    _organizationDTOEmail a == _organizationDTOEmail b &&
    _organizationDTORole a == _organizationDTORole b &&
    _organizationDTOToken a == _organizationDTOToken b &&
    _organizationDTOActive a == _organizationDTOActive b && _organizationDTOLogo a == _organizationDTOLogo b
