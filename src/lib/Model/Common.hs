module Model.Common where

class SameUuid e x where
  equalsUuid :: e -> x -> Bool
