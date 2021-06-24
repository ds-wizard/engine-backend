module Wizard.Metamodel.Migration.Migration7 where

import Data.Aeson

import qualified Wizard.Metamodel.Event.Version7 as V7
import qualified Wizard.Metamodel.Event.Version8 as V8

result2Either :: Result a -> Either String a
result2Either (Error msg) = Left msg
result2Either (Success x) = Right x

class Upgradeable f t where
  upgrade :: f -> Either String t

instance Upgradeable V7.Event V8.Event where
  upgrade = result2Either . fromJSON . toJSON

migrateEventValue :: Value -> Either String [Value]
migrateEventValue input = do
  oldEvent <- result2Either (fromJSON input)
  newEvent <- upgrade (oldEvent :: V7.Event)
  return [toJSON (newEvent :: V8.Event)]
