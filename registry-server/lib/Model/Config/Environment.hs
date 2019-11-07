module Model.Config.Environment where

import GHC.Generics

data Environment
  = Production
  | Staging
  | Development
  | Test
  deriving (Generic, Eq, Read, Show)
