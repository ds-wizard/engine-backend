module WizardLib.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateSM where

import Data.Swagger

import Shared.Common.Api.Resource.Common.SemVer2TupleSM ()
import Shared.Common.Util.Swagger
import WizardLib.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateDTO
import WizardLib.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateFormatSimpleSM ()
import WizardLib.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateJM ()
import WizardLib.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateAssets
import WizardLib.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFiles
import WizardLib.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFormats
import WizardLib.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateJM ()
import WizardLib.DocumentTemplate.Service.DocumentTemplate.DocumentTemplateMapper
import WizardLib.KnowledgeModel.Api.Resource.Package.PackagePatternSM ()

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
