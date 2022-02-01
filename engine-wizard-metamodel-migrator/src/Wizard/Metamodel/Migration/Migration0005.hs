module Wizard.Metamodel.Migration.Migration0005 where

import Data.Aeson

import qualified Wizard.Metamodel.Event.Version0005 as V5
import qualified Wizard.Metamodel.Event.Version0006 as V6
import Wizard.Metamodel.Migration.MigrationContext

result2Either :: Result a -> Either String a
result2Either (Error msg) = Left msg
result2Either (Success x) = Right x

class Upgradeable f t where
  upgrade :: f -> Either String t

instance Upgradeable V5.EventDTO V6.Event where
  upgrade = result2Either . fromJSON . toJSON

migrateEventValue :: MigrationContext -> Value -> Either String [Value]
migrateEventValue _ input = do
  oldEvent <- result2Either (fromJSON input)
  newEvent <- upgrade (oldEvent :: V5.EventDTO)
  return [toJSON (newEvent :: V6.Event)]
