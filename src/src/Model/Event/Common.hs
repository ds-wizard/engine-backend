module Model.Event.Common where

import Control.Lens

class SameUuid e x where
  equalsUuid :: e -> x -> Bool
