module Wizard.Database.Mapping.Branch.BranchDataLength where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

import Wizard.Model.Branch.BranchDataLength

instance FromRow BranchDataLength where
  fromRow = do
    _branchDataLengthBranchUuid <- field
    _branchDataLengthEventSize <- field
    return $ BranchDataLength {..}
