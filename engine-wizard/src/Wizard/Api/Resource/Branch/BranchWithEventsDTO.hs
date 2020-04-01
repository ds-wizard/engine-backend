module Wizard.Api.Resource.Branch.BranchWithEventsDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Api.Resource.Event.EventDTO

data BranchWithEventsDTO =
  BranchWithEventsDTO
    { _branchWithEventsDTOUuid :: U.UUID
    , _branchWithEventsDTOName :: String
    , _branchWithEventsDTOKmId :: String
    , _branchWithEventsDTOMetamodelVersion :: Int
    , _branchWithEventsDTOPreviousPackageId :: Maybe String
    , _branchWithEventsDTOForkOfPackageId :: Maybe String
    , _branchWithEventsDTOMergeCheckpointPackageId :: Maybe String
    , _branchWithEventsDTOEvents :: [EventDTO]
    , _branchWithEventsDTOOwnerUuid :: Maybe U.UUID
    , _branchWithEventsDTOCreatedAt :: UTCTime
    , _branchWithEventsDTOUpdatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)
