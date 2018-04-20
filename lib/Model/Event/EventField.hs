module Model.Event.EventField where

data EventField a = EventField
  { _eventFieldChanged :: Bool
  , _eventFieldValue :: a
  }
