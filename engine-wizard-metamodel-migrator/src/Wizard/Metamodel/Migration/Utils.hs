module Wizard.Metamodel.Migration.Utils where

import Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

type Key = T.Text

data BasicOp
  = Rename Key Key
  | Insert Key Value
  | Delete Key

runBasicOps :: [BasicOp] -> Object -> Object
runBasicOps ops obj = foldl (flip runBasicOp) obj ops

runBasicOp :: BasicOp -> Object -> Object
runBasicOp (Rename oldKey newKey) = renameKey oldKey newKey
runBasicOp (Insert key value) = HM.insert key value
runBasicOp (Delete key) = HM.delete key

renameKey :: Key -> Key -> Object -> Object
renameKey oldKey newKey obj =
  case HM.lookup oldKey obj of
    (Just v) -> HM.insert newKey v . HM.delete oldKey $ obj
    _ -> obj

migrateByEventType :: (T.Text -> Object -> Object) -> Value -> Value
migrateByEventType migrationFn v@(Object obj) =
  case HM.lookup "eventType" obj of
    (Just (String eventType)) -> Object (migrationFn eventType obj)
    _ -> v
migrateByEventType _ v = v

unchangedValue :: Value
unchangedValue = Object (HM.singleton "changed" (Bool False))
