module Wizard.Api.Resource.Branch.Event.BranchEventDTO where

import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Event.Event

data BranchEventDTO =
  AddBranchEventDTO' AddBranchEventDTO
  deriving (Show, Eq, Generic)

data AddBranchEventDTO =
  AddBranchEventDTO
    { _addBranchEventDTOUuid :: U.UUID
    , _addBranchEventDTOEvent :: Event
    }
  deriving (Show, Eq, Generic)
