module Wizard.Api.Resource.Package.PackageSuggestionSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Package.PackageSuggestionJM ()
import Wizard.Model.Package.PackageSuggestion
import Wizard.Service.Package.PackageMapper
import WizardLib.KnowledgeModel.Database.Migration.Development.Package.Data.Packages
import WizardLib.KnowledgeModel.Service.Package.PackageMapper

instance ToSchema PackageSuggestion where
  declareNamedSchema = toSwagger [toSuggestion (toPackage globalPackage)]
