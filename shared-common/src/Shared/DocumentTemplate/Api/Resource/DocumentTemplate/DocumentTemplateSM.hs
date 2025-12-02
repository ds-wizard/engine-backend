module Shared.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateSM where

import Data.Swagger

import Shared.Common.Api.Resource.Common.SemVer2TupleSM ()
import Shared.Common.Util.Swagger
import Shared.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateDTO
import Shared.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateFormatSimpleSM ()
import Shared.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateJM ()
import Shared.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateAssets
import Shared.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFiles
import Shared.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFormats
import Shared.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateJM ()
import Shared.DocumentTemplate.Service.DocumentTemplate.DocumentTemplateMapper
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackagePatternSM ()

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
  declareNamedSchema = toSwagger (toDTO wizardDocumentTemplate wizardDocumentTemplateFormats)

instance ToSchema DocumentTemplateFormatDTO where
  declareNamedSchema = toSwagger (toFormatDTO formatJson)

instance ToSchema DocumentTemplateFormatStepDTO where
  declareNamedSchema = toSwagger (toFormatStepDTO formatJsonStep)

instance ToSchema DocumentTemplateFileDTO where
  declareNamedSchema = toSwagger (toFileDTO fileDefaultHtml)

instance ToSchema DocumentTemplateAssetDTO where
  declareNamedSchema = toSwagger (toAssetDTO assetLogo)
