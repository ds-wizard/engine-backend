module Wizard.Api.Resource.Branch.BranchDetailDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Api.Resource.Event.EventDTO
import Wizard.Model.Branch.BranchState

data BranchDetailDTO =
  BranchDetailDTO
    { _branchDetailDTOUuid :: U.UUID
    , _branchDetailDTOName :: String
    , _branchDetailDTOKmId :: String
    , _branchDetailDTOState :: BranchState
    , _branchDetailDTOPreviousPackageId :: Maybe String
    , _branchDetailDTOForkOfPackageId :: Maybe String
    , _branchDetailDTOOwnerUuid :: Maybe U.UUID
    , _branchDetailDTOEvents :: [EventDTO]
    , _branchDetailDTOCreatedAt :: UTCTime
    , _branchDetailDTOUpdatedAt :: UTCTime
    }
  deriving (Generic)
