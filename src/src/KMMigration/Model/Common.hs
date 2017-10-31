module KMMigration.Model.Common where


type DateTime = String

class SameUuid e x where
  equalsUuid :: e -> x -> Bool
