module Wizard.Database.Migration.Production.Migration_0039_questionnaireAcl.Migration
  ( definition
  ) where

import Control.Monad.Logger hiding (logInfo)
import qualified Data.Bson as BSON
import Data.Foldable (traverse_)
import Data.Maybe (fromJust)
import Database.MongoDB hiding (createIndex)
import Database.MongoDB.Migration.Entity
import Database.Persist.MongoDB (ConnectionPool, runMongoDBPoolDef)

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 39, mmName = "Questionnaire ACL", mmDescription = "Add questionnaire ACL"}

migrate :: ConnectionPool -> LoggingT IO (Maybe Error)
migrate dbPool = do
  addGroupsToUsers dbPool
  addPermissionsForNonExistingOwner dbPool
  addPermissionsForOwner dbPool
  return Nothing

-- ------------------------------------------------------------------------------------------------
addGroupsToUsers dbPool = do
  let action = modify (select [] "users") ["$set" =: ["groups" =: ([] :: [String])]]
  runMongoDBPoolDef action dbPool

-- ------------------------------------------------------------------------------------------------
addPermissionsForOwner dbPool = do
  let action = rest =<< find (select ["ownerUuid" =: ["$ne" =: (Nothing :: Maybe String)]] "questionnaires")
  questionnaires <- runMongoDBPoolDef action dbPool
  traverse_ (addPermissionForOwner dbPool) questionnaires

addPermissionForOwner dbPool qtn = do
  let qtnUuid = extractUuid qtn
  let ownerUuid = extractOwnerUuid qtn
  let action =
        modify
          (select ["uuid" =: qtnUuid] "questionnaires")
          [ "$set" =:
            [ "permissions" =:
              [["member" =: ["type" =: "UserMember", "uuid" =: ownerUuid], "perms" =: ["ADMIN", "EDIT", "VIEW"]]]
            ]
          , "$unset" =: ["ownerUuid" =: ""]
          ]
  runMongoDBPoolDef action dbPool
  return Nothing

-- ------------------------------------------------------------------------------------------------
addPermissionsForNonExistingOwner dbPool = do
  let actionFind = rest =<< find (select ["role" =: "admin"] "users")
  users <- runMongoDBPoolDef actionFind dbPool
  let userUuids = fmap extractUuid users
  let permissions = fmap createPermission userUuids
  let actionModify =
        modify
          (select ["ownerUuid" =: (Nothing :: Maybe String)] "questionnaires")
          ["$set" =: ["permissions" =: permissions], "$unset" =: ["ownerUuid" =: ""]]
  runMongoDBPoolDef actionModify dbPool
  return Nothing

createPermission userUuid =
  ["member" =: ["type" =: "UserMember", "uuid" =: userUuid], "perms" =: ["ADMIN", "EDIT", "VIEW"]]

-- ------------------------------------------------
-- Extractor
-- ------------------------------------------------
extractUuid :: BSON.Document -> String
extractUuid = fromJust . BSON.lookup "uuid"

extractOwnerUuid :: BSON.Document -> String
extractOwnerUuid = fromJust . BSON.lookup "ownerUuid"
