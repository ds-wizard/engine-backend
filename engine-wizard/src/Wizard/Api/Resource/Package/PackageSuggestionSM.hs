module Wizard.Api.Resource.Package.PackageSuggestionSM where

import Data.Swagger

import Shared.Database.Migration.Development.Package.Data.Packages
import Shared.Service.Package.PackageMapper
import Shared.Util.Swagger
import Wizard.Api.Resource.Package.PackageSuggestionJM ()
import Wizard.Model.Package.PackageSuggestion
import Wizard.Service.Package.PackageMapper

instance ToSchema PackageSuggestion where
  declareNamedSchema = toSwagger [toSuggestion (toPackage globalPackage, [])]
