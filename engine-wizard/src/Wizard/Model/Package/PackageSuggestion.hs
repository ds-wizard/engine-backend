module Wizard.Model.Package.PackageSuggestion where

import GHC.Generics

data PackageSuggestion =
  PackageSuggestion
    { _packageSuggestionPId :: String
    , _packageSuggestionName :: String
    , _packageSuggestionVersion :: String
    , _packageSuggestionDescription :: String
    , _packageSuggestionVersions :: [String]
    }
  deriving (Show, Eq, Ord, Generic)
