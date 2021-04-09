module Wizard.Metamodel.Migration.Migration6 where

import Data.Aeson

import qualified Wizard.Metamodel.Event.Version6 as V6
import qualified Wizard.Metamodel.Event.Version7 as V7

result2Either :: Result a -> Either String a
result2Either (Error msg) = Left msg
result2Either (Success x) = Right x

class Upgradeable f t where
  upgrade :: f -> Either String t

instance Upgradeable V6.Event V7.Event where
  upgrade = result2Either . fromJSON . toJSON

migrateEventValue :: Value -> Either String Value
migrateEventValue input = do
  oldEvent <- result2Either (fromJSON input)
  newEvent <- upgrade (oldEvent :: V6.Event)
  return $ toJSON (newEvent :: V7.Event)
