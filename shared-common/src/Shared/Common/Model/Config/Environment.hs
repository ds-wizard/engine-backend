module Shared.Common.Model.Config.Environment where

import GHC.Generics

data Environment
  = Production
  | Development
  | Test
  deriving (Generic, Eq, Read, Show)
