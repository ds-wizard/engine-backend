module Wizard.Model.Package.PackageSuggestion where

import GHC.Generics

data PackageSuggestion = PackageSuggestion
  { pId :: String
  , name :: String
  , version :: String
  , description :: String
  }
  deriving (Show, Eq, Ord, Generic)
