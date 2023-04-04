module Wizard.Model.Branch.BranchList where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Model.Branch.BranchState

data BranchList = BranchList
  { uuid :: U.UUID
  , name :: String
  , kmId :: String
  , version :: String
  , state :: BranchState
  , previousPackageId :: Maybe String
  , forkOfPackageId :: Maybe String
  , createdBy :: Maybe U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Generic)

instance Eq BranchList where
  a == b =
    uuid a == uuid b
      && name a == name b
      && kmId a == kmId b
      && version a == version b
      && state a == state b
      && previousPackageId a == previousPackageId b
      && forkOfPackageId a == forkOfPackageId b
      && createdBy a == createdBy b
