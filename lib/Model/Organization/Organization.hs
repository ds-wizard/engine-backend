module Model.Organization.Organization where

import Control.Lens
import Control.Lens.Traversal
import Data.List
import Data.UUID
import GHC.Generics

data Organization = Organization
  { _organizationUuid :: UUID
  , _organizationName :: String
  , _organizationOrganizationId :: String
  } deriving (Show, Eq, Generic)
