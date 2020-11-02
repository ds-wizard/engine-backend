module Shared.Api.Resource.Package.PackageSuggestionDTO where

import GHC.Generics

data PackageSuggestionDTO =
  PackageSuggestionDTO
    { _packageSuggestionDTOPId :: String
    , _packageSuggestionDTOName :: String
    , _packageSuggestionDTOVersion :: String
    , _packageSuggestionDTODescription :: String
    , _packageSuggestionDTOVersions :: [String]
    }
  deriving (Show, Eq, Generic)
