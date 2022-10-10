module Wizard.Database.Mapping.Branch.BranchList where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

import Wizard.Database.Mapping.Branch.BranchState ()
import Wizard.Model.Branch.BranchList

instance FromRow BranchList where
  fromRow = do
    _branchListUuid <- field
    _branchListName <- field
    _branchListKmId <- field
    _branchListState <- field
    _branchListPreviousPackageId <- field
    _branchListForkOfPackageId <- field
    _branchListCreatedBy <- field
    _branchListCreatedAt <- field
    _branchListUpdatedAt <- field
    return $ BranchList {..}
