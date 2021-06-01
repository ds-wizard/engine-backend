module Shared.Api.Resource.Package.PackageSuggestionSM where

import Data.Swagger

import Shared.Api.Resource.Package.PackageSuggestionDTO
import Shared.Api.Resource.Package.PackageSuggestionJM ()
import Shared.Database.Migration.Development.Package.Data.Packages
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Shared.Service.Package.PackageMapper
import Shared.Util.Swagger

instance ToSchema PackageSuggestionDTO where
  declareNamedSchema =
    simpleToSchema (toSuggestionDTO (toPackage netherlandsPackage) (Page "packages" (PageMetadata 20 0 0 0) []))
