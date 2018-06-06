module Model.Branch.Branch where

import Data.UUID
import GHC.Generics

import Model.Event.Event
import Model.KnowledgeModel.KnowledgeModel

data Branch = Branch
  { _branchUuid :: UUID
  , _branchName :: String
  , _branchKmId :: String
  , _branchParentPackageId :: Maybe String
  , _branchLastAppliedParentPackageId :: Maybe String
  , _branchLastMergeCheckpointPackageId :: Maybe String
  } deriving (Show, Eq, Generic)

data BranchWithEvents = BranchWithEvents
  { _branchWithEventsUuid :: UUID
  , _branchWithEventsName :: String
  , _branchWithEventsKmId :: String
  , _branchWithEventsParentPackageId :: Maybe String
  , _branchWithEventsLastAppliedParentPackageId :: Maybe String
  , _branchWithEventsLastMergeCheckpointPackageId :: Maybe String
  , _branchWithEventsEvents :: [Event]
  } deriving (Generic)

data BranchWithKM = BranchWithKM
  { _branchWithKMUuid :: UUID
  , _branchWithKMName :: String
  , _branchWithKMKmId :: String
  , _branchWithKMParentPackageId :: Maybe String
  , _branchWithKMLastAppliedParentPackageId :: Maybe String
  , _branchWithKMLastMergeCheckpointPackageId :: Maybe String
  , _branchWithKMKnowledgeModel :: Maybe KnowledgeModel
  } deriving (Show, Eq, Generic)
