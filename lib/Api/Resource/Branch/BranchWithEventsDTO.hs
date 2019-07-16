module Api.Resource.Branch.BranchWithEventsDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Api.Resource.Event.EventDTO

data BranchWithEventsDTO = BranchWithEventsDTO
  { _branchWithEventsDTOUuid :: U.UUID
  , _branchWithEventsDTOName :: String
  , _branchWithEventsDTOKmId :: String
  , _branchWithEventsDTOMetamodelVersion :: Int
  , _branchWithEventsDTOParentPackageId :: Maybe String
  , _branchWithEventsDTOLastAppliedParentPackageId :: Maybe String
  , _branchWithEventsDTOLastMergeCheckpointPackageId :: Maybe String
  , _branchWithEventsDTOEvents :: [EventDTO]
  , _branchWithEventsDTOOwnerUuid :: Maybe U.UUID
  , _branchWithEventsDTOCreatedAt :: UTCTime
  , _branchWithEventsDTOUpdatedAt :: UTCTime
  } deriving (Show, Eq, Generic)
