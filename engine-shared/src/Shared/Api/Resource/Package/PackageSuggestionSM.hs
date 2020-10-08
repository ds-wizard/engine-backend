module Shared.Api.Resource.Package.PackageSuggestionSM where

import Data.Swagger

import Shared.Api.Resource.Package.PackageSuggestionDTO
import Shared.Api.Resource.Package.PackageSuggestionJM ()
import Shared.Database.Migration.Development.Package.Data.Packages
import Shared.Service.Package.PackageMapper
import Shared.Util.Swagger

instance ToSchema PackageSuggestionDTO where
  declareNamedSchema = simpleToSchema (toSuggestionDTO netherlandsPackageGroup)
