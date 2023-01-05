module Shared.Api.Resource.DocumentTemplate.DocumentTemplateSM where

import Data.Swagger

import Shared.Api.Resource.DocumentTemplate.DocumentTemplateDTO
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateFormatSM ()
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateJM ()
import Shared.Api.Resource.Package.PackagePatternSM ()
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateAssets
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFiles
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFormats
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplateJM ()
import Shared.Service.DocumentTemplate.DocumentTemplateMapper
import Shared.Util.Swagger

instance ToSchema DocumentTemplatePhase

instance ToSchema DocumentTemplate where
  declareNamedSchema = toSwagger wizardDocumentTemplate

instance ToSchema DocumentTemplateFormat where
  declareNamedSchema = toSwagger formatJson

instance ToSchema DocumentTemplateFormatStep where
  declareNamedSchema = toSwagger formatJsonStep

instance ToSchema DocumentTemplateFile where
  declareNamedSchema = toSwagger fileDefaultHtml

instance ToSchema DocumentTemplateAsset where
  declareNamedSchema = toSwagger assetLogo

instance ToSchema DocumentTemplateDTO where
  declareNamedSchema = toSwagger (toDTO wizardDocumentTemplate)

instance ToSchema DocumentTemplateFileDTO where
  declareNamedSchema = toSwagger (toFileDTO fileDefaultHtml)

instance ToSchema DocumentTemplateAssetDTO where
  declareNamedSchema = toSwagger (toAssetDTO assetLogo)
