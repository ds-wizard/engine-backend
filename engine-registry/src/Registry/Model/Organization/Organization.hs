module Registry.Model.Organization.Organization where

import Data.Time
import GHC.Generics

data OrganizationRole
  = AdminRole
  | UserRole
  deriving (Show, Eq, Generic)

data Organization =
  Organization
    { _organizationOrganizationId :: String
    , _organizationName :: String
    , _organizationDescription :: String
    , _organizationEmail :: String
    , _organizationRole :: OrganizationRole
    , _organizationToken :: String
    , _organizationActive :: Bool
    , _organizationLogo :: Maybe String
    , _organizationCreatedAt :: UTCTime
    , _organizationUpdatedAt :: UTCTime
    }
  deriving (Show, Generic)

instance Eq Organization where
  a == b =
    _organizationOrganizationId a == _organizationOrganizationId b &&
    _organizationName a == _organizationName b &&
    _organizationDescription a == _organizationDescription b &&
    _organizationEmail a == _organizationEmail b &&
    _organizationRole a == _organizationRole b &&
    _organizationToken a == _organizationToken b &&
    _organizationActive a == _organizationActive b && _organizationLogo a == _organizationLogo b
