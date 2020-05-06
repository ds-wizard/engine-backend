module Wizard.Database.Migration.Production.Migration_0027_submission.Migration
  ( definition
  ) where

import Control.Monad.Logger
import Database.MongoDB
import Database.MongoDB.Migration.Entity
import Database.Persist.MongoDB (ConnectionPool, runMongoDBPoolDef)

definition = (meta, migrate)

meta =
  MigrationMeta
    { mmNumber = 27
    , mmName = "Submission"
    , mmDescription = "Add config for submission into DB and add SUBM_PERM to all users"
    }

migrate :: ConnectionPool -> LoggingT IO (Maybe Error)
migrate dbPool = do
  updateUser dbPool
  addSubmissionConfig dbPool
  return Nothing

updateUser dbPool = do
  let action =
        modify
          (select [] "users")
          ["$push" =: ["permissions" =: "SUBM_PERM"], "$set" =: ["submissionProps" =: ([] :: [String])]]
  runMongoDBPoolDef action dbPool

addSubmissionConfig dbPool = do
  let actionModify =
        modify
          (select [] "appConfigs")
          ["$set" =: ["submission" =: ["enabled" =: True, "services" =: ([] :: [String])]]]
  runMongoDBPoolDef actionModify dbPool
