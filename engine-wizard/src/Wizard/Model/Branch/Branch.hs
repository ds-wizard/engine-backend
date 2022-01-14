module Wizard.Model.Branch.Branch where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data Branch =
  Branch
    { _branchUuid :: U.UUID
    , _branchName :: String
    , _branchKmId :: String
    , _branchPreviousPackageId :: Maybe String
    , _branchOwnerUuid :: Maybe U.UUID
    , _branchAppUuid :: U.UUID
    , _branchCreatedAt :: UTCTime
    , _branchUpdatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)
