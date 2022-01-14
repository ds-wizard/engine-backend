module Wizard.Metamodel.Migration.Migration0006 where

import Data.Aeson

import qualified Wizard.Metamodel.Event.Version0006 as V6
import qualified Wizard.Metamodel.Event.Version0007 as V7
import Wizard.Metamodel.Migration.MigrationContext

result2Either :: Result a -> Either String a
result2Either (Error msg) = Left msg
result2Either (Success x) = Right x

class Upgradeable f t where
  upgrade :: f -> Either String t

instance Upgradeable V6.Event V7.Event where
  upgrade = result2Either . fromJSON . toJSON

migrateEventValue :: MigrationContext -> Value -> Either String [Value]
migrateEventValue _ input = do
  oldEvent <- result2Either (fromJSON input)
  newEvent <- upgrade (oldEvent :: V6.Event)
  return [toJSON (newEvent :: V7.Event)]
