module Wizard.Model.Package.PackageSuggestion where

import GHC.Generics

data PackageSuggestion = PackageSuggestion
  { pId :: String
  , name :: String
  , version :: String
  , description :: String
  , versions :: [String]
  }
  deriving (Show, Eq, Ord, Generic)
