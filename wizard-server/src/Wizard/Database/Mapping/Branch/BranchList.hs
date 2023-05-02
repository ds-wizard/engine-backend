module Wizard.Database.Mapping.Branch.BranchList where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

import Wizard.Database.Mapping.Branch.BranchState ()
import Wizard.Model.Branch.BranchList

instance FromRow BranchList where
  fromRow = do
    uuid <- field
    name <- field
    kmId <- field
    version <- field
    state <- field
    previousPackageId <- field
    forkOfPackageId <- field
    createdBy <- field
    createdAt <- field
    updatedAt <- field
    return $ BranchList {..}
