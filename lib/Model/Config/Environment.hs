module Model.Config.Environment where

data Environment
  = Production
  | Staging
  | Development
  | Test
  deriving (Eq, Read, Show)
