module Wizard.Service.KnowledgeModel.Metamodel.Migrator.Migrations.Migration0018 (
  migrateEventValue,
) where

import Data.Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe

import Wizard.Service.KnowledgeModel.Metamodel.Migrator.Migrations.MigrationContext

-- Migration #0018 (KM v18 -> v19)
-- . Transform a JSON object so that:
--   - if the object does not contain "uuid", it is returned unchanged
--   - otherwise:
--       - "uuid", "parentUuid", "entityUuid", "createdAt" remain at the top level
--       - all other fields go under a "content" object
migrateEventValue :: MigrationContext -> Value -> Either a [Value]
migrateEventValue _ input = Right [migrate input]

migrate :: Value -> Value
migrate (Object obj) =
  if isNothing (KM.lookup (K.fromText "uuid") obj)
    then Object obj
    else
      let topKeys = map K.fromText ["uuid", "parentUuid", "entityUuid", "createdAt"]
          isTopKey k = k `elem` topKeys
          topLevel = KM.filterWithKey (\k _ -> isTopKey k) obj
          rest = KM.filterWithKey (\k _ -> not (isTopKey k)) obj
          newObj = KM.insert (K.fromText "content") (Object rest) topLevel
       in Object newObj
migrate v = v
