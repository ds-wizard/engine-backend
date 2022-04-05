module Shared.Model.Event.EventField where

import Data.Typeable
import GHC.Generics

data EventField a
  = NothingChanged
  | ChangedValue a
  deriving (Show, Eq, Generic, Typeable)

instance Functor EventField where
  fmap f (ChangedValue a) = ChangedValue (f a)
  fmap _ NothingChanged = NothingChanged

isChangedValue :: EventField a -> Bool
isChangedValue NothingChanged = False
isChangedValue _ = True

diffField :: Eq field => field -> field -> EventField field
diffField oldField newField =
  if oldField /= newField
    then ChangedValue newField
    else NothingChanged
