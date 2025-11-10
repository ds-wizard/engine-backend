module Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSuggestionSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSuggestionJM ()
import Wizard.Model.KnowledgeModel.Package.KnowledgeModelPackageSuggestion
import Wizard.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper

instance ToSchema KnowledgeModelPackageSuggestion where
  declareNamedSchema = toSwagger [toSuggestion globalKmPackage]
