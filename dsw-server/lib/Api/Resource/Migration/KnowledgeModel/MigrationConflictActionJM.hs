module Api.Resource.Migration.KnowledgeModel.MigrationConflictActionJM where

import Control.Monad
import Data.Aeson

import Model.Migration.KnowledgeModel.MigratorState

instance FromJSON MigrationConflictAction where
  parseJSON (String "Apply") = return MCAApply
  parseJSON (String "Edited") = return MCAEdited
  parseJSON (String "Reject") = return MCAReject
  parseJSON _ = mzero

instance ToJSON MigrationConflictAction where
  toJSON MCAApply = "Apply"
  toJSON MCAEdited = "Edited"
  toJSON MCAReject = "Reject"
