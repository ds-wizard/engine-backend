module Wizard.Database.BSON.Branch.Branch where

import Data.Bson.Generic

import Shared.Database.BSON.Common ()
import Wizard.Model.Branch.Branch

instance ToBSON Branch

instance FromBSON Branch
