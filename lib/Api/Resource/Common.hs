module Api.Resource.Common where

import Model.Branch.BranchState

serializeBranchState :: BranchState -> String
serializeBranchState BSDefault = "Default"
serializeBranchState BSEdited = "Edited"
serializeBranchState BSOutdated = "Outdated"
serializeBranchState BSMigrating = "Migrating"
serializeBranchState BSMigrated = "Migrated"

deserializeBranchState :: String -> Maybe BranchState
deserializeBranchState "Default" = Just BSDefault
deserializeBranchState "Edited" = Just BSEdited
deserializeBranchState "Outdated" = Just BSOutdated
deserializeBranchState "Migrating" = Just BSMigrating
deserializeBranchState "Migrated" = Just BSMigrated
deserializeBranchState _ = Nothing
