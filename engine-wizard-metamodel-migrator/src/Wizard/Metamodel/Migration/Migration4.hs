module Wizard.Metamodel.Migration.Migration4 where

import Data.Aeson

import qualified Wizard.Metamodel.Event.Version4 as V4
import qualified Wizard.Metamodel.Event.Version5 as V5

result2Either :: Result a -> Either String a
result2Either (Error msg) = Left msg
result2Either (Success x) = Right x

class Upgradeable f t where
  upgrade :: f -> Either String t

instance Upgradeable V4.EventDTO V5.EventDTO where
  upgrade = result2Either . fromJSON . toJSON

migrateEventValue :: Value -> Either String [Value]
migrateEventValue input = do
  oldEvent <- result2Either (fromJSON input)
  newEvent <- upgrade (oldEvent :: V4.EventDTO)
  return [toJSON (newEvent :: V5.EventDTO)]
