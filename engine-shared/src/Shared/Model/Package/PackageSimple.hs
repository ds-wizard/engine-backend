module Shared.Model.Package.PackageSimple where

import GHC.Generics

data PackageSimple =
  PackageSimple
    { _packageSimplePId :: String
    , _packageSimpleName :: String
    , _packageSimpleVersion :: String
    }
  deriving (Generic, Eq, Show)
