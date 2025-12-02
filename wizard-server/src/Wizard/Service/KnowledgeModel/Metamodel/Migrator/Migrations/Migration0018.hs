module Wizard.Service.KnowledgeModel.Metamodel.Migrator.Migrations.Migration0018 (
  migrateEventValue,
) where

import Data.Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe

import Wizard.Service.KnowledgeModel.Metamodel.Migrator.Migrations.MigrationContext

-- Migration #0018 (KM v18 -> v19)
-- Transform a JSON object as follows:
--   - If the object does not contain "uuid", it is returned unchanged.
--   - If the object contains "content", it is returned unchanged *except* in these cases:
--       1. If "eventType" = "AddResourcePageEvent" and "content" is a String, the object is migrated.
--       2. If "eventType" = "EditResourcePageEvent" and "content" is an Object containing the field "changed",
--          the object is migrated.
--     (In all other cases where "content" exists, the object is returned unchanged.)
--   - If the object should be migrated:
--       - "uuid", "parentUuid", "entityUuid", and "createdAt" remain at the top level.
--       - All remaining fields are moved under a new "content" object.
migrateEventValue :: MigrationContext -> Value -> Either a [Value]
migrateEventValue _ input = Right [migrate input]

migrate :: Value -> Value
migrate (Object obj)
  | isNothing (KM.lookup (K.fromText "uuid") obj) = Object obj
  | hasContentAndShouldKeep obj = Object obj
  | otherwise =
      let topKeys = map K.fromText ["uuid", "parentUuid", "entityUuid", "createdAt"]
          isTopKey k = k `elem` topKeys
          topLevel = KM.filterWithKey (\k _ -> isTopKey k) obj
          rest = KM.filterWithKey (\k _ -> not (isTopKey k)) obj
          newObj = KM.insert (K.fromText "content") (Object rest) topLevel
       in Object newObj
  where
    hasContentAndShouldKeep :: KM.KeyMap Value -> Bool
    hasContentAndShouldKeep o =
      case KM.lookup (K.fromText "content") o of
        Nothing -> False
        Just contentVal ->
          case KM.lookup (K.fromText "eventType") o of
            -- 1. eventType == AddResourcePageEvent && content is String
            Just (String "AddResourcePageEvent")
              | String _ <- contentVal ->
                  False
            -- 2. eventType == EditResourcePageEvent && content is Object with field "changed"
            Just (String "EditResourcePageEvent")
              | Object contentObj <- contentVal
              , KM.member (K.fromText "changed") contentObj ->
                  False
            -- 3. All other cases:
            --    apply the "has content" condition -> return Object obj
            _ -> True
migrate v = v
