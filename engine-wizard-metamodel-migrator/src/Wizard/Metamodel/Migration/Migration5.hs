module Wizard.Metamodel.Migration.Migration5 where

import Data.Aeson

import qualified Wizard.Metamodel.Event.Version5 as V5
import qualified Wizard.Metamodel.Event.Version6 as V6

result2Either :: Result a -> Either String a
result2Either (Error msg) = Left msg
result2Either (Success x) = Right x

class Upgradeable f t where
  upgrade :: f -> Either String t

instance Upgradeable V5.EventDTO V6.Event where
  upgrade = result2Either . fromJSON . toJSON

migrateEventValue :: Value -> Either String [Value]
migrateEventValue input = do
  oldEvent <- result2Either (fromJSON input)
  newEvent <- upgrade (oldEvent :: V5.EventDTO)
  return [toJSON (newEvent :: V6.Event)]
