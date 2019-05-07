module Model.Branch.Branch where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Model.Event.Event

data Branch = Branch
  { _branchUuid :: U.UUID
  , _branchName :: String
  , _branchKmId :: String
  , _branchMetamodelVersion :: Int
  , _branchParentPackageId :: Maybe String
  , _branchLastAppliedParentPackageId :: Maybe String
  , _branchLastMergeCheckpointPackageId :: Maybe String
  , _branchOwnerUuid :: Maybe U.UUID
  , _branchCreatedAt :: UTCTime
  , _branchUpdatedAt :: UTCTime
  } deriving (Show, Eq, Generic)

data BranchWithEvents = BranchWithEvents
  { _branchWithEventsUuid :: U.UUID
  , _branchWithEventsName :: String
  , _branchWithEventsKmId :: String
  , _branchWithEventsMetamodelVersion :: Int
  , _branchWithEventsParentPackageId :: Maybe String
  , _branchWithEventsLastAppliedParentPackageId :: Maybe String
  , _branchWithEventsLastMergeCheckpointPackageId :: Maybe String
  , _branchWithEventsEvents :: [Event]
  , _branchWithEventsOwnerUuid :: Maybe U.UUID
  , _branchWithEventsCreatedAt :: UTCTime
  , _branchWithEventsUpdatedAt :: UTCTime
  } deriving (Generic)
