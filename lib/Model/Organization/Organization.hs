module Model.Organization.Organization where

import Data.Time
import Data.UUID
import GHC.Generics

data Organization = Organization
  { _organizationUuid :: UUID
  , _organizationName :: String
  , _organizationOrganizationId :: String
  , _organizationCreatedAt :: UTCTime
  , _organizationUpdatedAt :: UTCTime
  } deriving (Show, Generic)

instance Eq Organization where
  a == b =
    _organizationUuid a == _organizationUuid b &&
    _organizationName a == _organizationName b && _organizationOrganizationId a == _organizationOrganizationId b
