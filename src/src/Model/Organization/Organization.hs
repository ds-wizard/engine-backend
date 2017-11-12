module Model.Organization.Organization where

import Control.Lens
import Control.Lens.Traversal
import Data.List
import Data.UUID
import GHC.Generics

data Organization = Organization
  { _orgUuid :: UUID
  , _orgName :: String
  , _orgGroupId :: String
  } deriving (Show, Eq, Generic)

makeLenses ''Organization
