module Wizard.Service.KnowledgeModel.Metamodel.Migrator.Migrations.Utils where

import Data.Aeson hiding (Key)
import Data.Aeson.Key (fromText)
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

type Key = T.Text

data BasicOp
  = Rename Key Key
  | Insert Key Value
  | Delete Key
  | Change Key (Value -> Value)

runBasicOps :: [BasicOp] -> Object -> Object
runBasicOps ops obj = foldl (flip runBasicOp) obj ops

runBasicOp :: BasicOp -> Object -> Object
runBasicOp (Rename oldKey newKey) = renameKey oldKey newKey
runBasicOp (Insert key value) = KM.insert (fromText key) value
runBasicOp (Delete key) = KM.delete (fromText key)
runBasicOp (Change key fn) = change
  where
    change obj =
      case KM.lookup (fromText key) obj of
        (Just value) -> KM.insert (fromText key) (fn value) obj
        _ -> obj

renameKey :: Key -> Key -> Object -> Object
renameKey oldKey newKey obj =
  case KM.lookup (fromText oldKey) obj of
    (Just v) -> KM.insert (fromText newKey) v . KM.delete (fromText oldKey) $ obj
    _ -> obj

migrateByEventType :: (T.Text -> Object -> Object) -> Value -> Value
migrateByEventType migrationFn v@(Object obj) =
  case KM.lookup "eventType" obj of
    (Just (String eventType)) -> Object (migrationFn eventType obj)
    _ -> v
migrateByEventType _ v = v

migrateByEventTypeMaybe :: (T.Text -> Object -> Maybe Object) -> Value -> Maybe Value
migrateByEventTypeMaybe migrationFn v@(Object obj) =
  case KM.lookup "eventType" obj of
    (Just (String eventType)) ->
      case migrationFn eventType obj of
        Just newObj -> Just (Object newObj)
        Nothing -> Nothing
    _ -> Nothing
migrateByEventTypeMaybe _ v = Nothing

migrateEventContent :: (Value -> Value) -> Value -> Value
migrateEventContent contentMigrationFn v@(Object obj) =
  case KM.lookup "content" obj of
    (Just contentVal) ->
      let newContent = contentMigrationFn contentVal
       in Object (KM.insert "content" newContent obj)
    _ -> v
migrateEventContent _ v = v

migrateEventContentMaybe :: (Value -> Maybe Value) -> Value -> Maybe Value
migrateEventContentMaybe contentMigrationFn v@(Object obj) =
  case KM.lookup "content" obj of
    (Just contentVal) ->
      case contentMigrationFn contentVal of
        Just newContent -> Just $ Object (KM.insert "content" newContent obj)
        Nothing -> Nothing
    _ -> Nothing
migrateEventContentMaybe _ v = Nothing

unchangedValue :: Value
unchangedValue = Object (KM.singleton "changed" (Bool False))

nullUuid :: Value
nullUuid = String "00000000-0000-0000-0000-000000000000"

applyOnEventField :: (Value -> Value) -> Value -> Value
applyOnEventField fn v@(Object obj) =
  case KM.lookup "value" obj of
    (Just value) -> Object $ KM.insert "value" (fn value) obj
    _ -> v
applyOnEventField _ v = v

startsWith :: T.Text -> T.Text -> Bool
startsWith text prefix
  | T.length text < prefixSize = False
  | otherwise = T.take prefixSize text == prefix
  where
    prefixSize = T.length prefix

endsWith :: T.Text -> T.Text -> Bool
endsWith text suffix
  | T.length text < suffixSize = False
  | otherwise = T.takeEnd suffixSize text == suffix
  where
    suffixSize = T.length suffix

chainMigrations :: [Value -> Value] -> (Value -> Value)
chainMigrations = foldl (.) id

extractValue :: KM.Key -> Object -> Value
extractValue key = fromMaybe unchangedValue . KM.lookup key
