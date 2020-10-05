module Wizard.Database.BSON.Branch.BranchWithEvents where

import Data.Bson.Generic

import Shared.Database.BSON.Common ()
import Shared.Database.BSON.Event.Common ()
import Wizard.Model.Branch.Branch

instance ToBSON BranchWithEvents

instance FromBSON BranchWithEvents
