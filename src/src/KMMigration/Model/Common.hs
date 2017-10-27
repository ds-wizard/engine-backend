module KMMigration.Model.Common where

type UUID = String

type DateTime = String

class SameUuid e x where
  equalsUuid :: e -> x -> Bool
