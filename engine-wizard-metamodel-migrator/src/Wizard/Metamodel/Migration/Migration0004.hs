module Wizard.Metamodel.Migration.Migration0004 where

import Data.Aeson

import qualified Wizard.Metamodel.Event.Version0004 as V4
import qualified Wizard.Metamodel.Event.Version0005 as V5
import Wizard.Metamodel.Migration.MigrationContext

result2Either :: Result a -> Either String a
result2Either (Error msg) = Left msg
result2Either (Success x) = Right x

class Upgradeable f t where
  upgrade :: f -> Either String t

instance Upgradeable V4.EventDTO V5.EventDTO where
  upgrade = result2Either . fromJSON . toJSON

migrateEventValue :: MigrationContext -> Value -> Either String [Value]
migrateEventValue _ input = do
  oldEvent <- result2Either (fromJSON input)
  newEvent <- upgrade (oldEvent :: V4.EventDTO)
  return [toJSON (newEvent :: V5.EventDTO)]
