module Model.Branch.Branch where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Model.Event.Event
import Model.KnowledgeModel.KnowledgeModel

data Branch = Branch
  { _branchUuid :: U.UUID
  , _branchName :: String
  , _branchKmId :: String
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
  , _branchWithEventsParentPackageId :: Maybe String
  , _branchWithEventsLastAppliedParentPackageId :: Maybe String
  , _branchWithEventsLastMergeCheckpointPackageId :: Maybe String
  , _branchWithEventsEvents :: [Event]
  , _branchWithEventsOwnerUuid :: Maybe U.UUID
  , _branchWithEventsCreatedAt :: UTCTime
  , _branchWithEventsUpdatedAt :: UTCTime
  } deriving (Generic)

data BranchWithKM = BranchWithKM
  { _branchWithKMUuid :: U.UUID
  , _branchWithKMName :: String
  , _branchWithKMKmId :: String
  , _branchWithKMParentPackageId :: Maybe String
  , _branchWithKMLastAppliedParentPackageId :: Maybe String
  , _branchWithKMLastMergeCheckpointPackageId :: Maybe String
  , _branchWithKMKnowledgeModel :: Maybe KnowledgeModel
  , _branchWithKMOwnerUuid :: Maybe U.UUID
  , _branchWithKMCreatedAt :: UTCTime
  , _branchWithKMUpdatedAt :: UTCTime
  } deriving (Show, Eq, Generic)
