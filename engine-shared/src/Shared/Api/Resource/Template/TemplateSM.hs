module Shared.Api.Resource.Template.TemplateSM where

import Data.Swagger

import Shared.Api.Resource.Package.PackagePatternSM ()
import Shared.Api.Resource.Template.TemplateDTO
import Shared.Api.Resource.Template.TemplateFormatSM ()
import Shared.Api.Resource.Template.TemplateJM ()
import Shared.Database.Migration.Development.Template.Data.Templates
import Shared.Model.Template.Template
import Shared.Model.Template.TemplateJM ()
import Shared.Service.Template.TemplateMapper
import Shared.Util.Swagger

instance ToSchema Template where
  declareNamedSchema = toSwagger commonWizardTemplate

instance ToSchema TemplateFormat where
  declareNamedSchema = toSwagger templateFormatJson

instance ToSchema TemplateFormatStep where
  declareNamedSchema = toSwagger templateFormatJsonStep

instance ToSchema TemplateFile where
  declareNamedSchema = toSwagger templateFileDefaultHtml

instance ToSchema TemplateAsset where
  declareNamedSchema = toSwagger templateAssetLogo

instance ToSchema TemplateDTO where
  declareNamedSchema = toSwagger (toDTO commonWizardTemplate)

instance ToSchema TemplateFileDTO where
  declareNamedSchema = toSwagger (toFileDTO templateFileDefaultHtml)

instance ToSchema TemplateAssetDTO where
  declareNamedSchema = toSwagger (toAssetDTO templateAssetLogo)
