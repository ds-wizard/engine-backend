module Api.Resource.Branch.BranchStateJM where

import Data.Aeson

import Model.Branch.BranchState

instance ToJSON BranchState where
  toJSON BSDefault = toJSON "Default"
  toJSON BSEdited = toJSON "Edited"
  toJSON BSOutdated = toJSON "Outdated"
  toJSON BSMigrating = toJSON "Migrating"
  toJSON BSMigrated = toJSON "Migrated"

instance FromJSON BranchState where
  parseJSON (String "Default") = return BSDefault
  parseJSON (String "Edited") = return BSEdited
  parseJSON (String "Outdated") = return BSOutdated
  parseJSON (String "Migrating") = return BSMigrating
  parseJSON (String "Migrated") = return BSMigrated
  parseJSON _ = fail "Unsupported BranchState"
