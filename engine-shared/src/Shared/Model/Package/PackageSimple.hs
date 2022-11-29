module Shared.Model.Package.PackageSimple where

import GHC.Generics

data PackageSimple = PackageSimple
  { pId :: String
  , name :: String
  , version :: String
  }
  deriving (Generic, Eq, Show)
