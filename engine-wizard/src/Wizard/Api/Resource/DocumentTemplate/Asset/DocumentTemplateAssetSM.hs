module Wizard.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetSM where

import Data.Swagger

import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateAssets
import Shared.Util.Swagger
import Wizard.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetDTO
import Wizard.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetJM ()
import Wizard.Service.DocumentTemplate.Asset.DocumentTemplateAssetMapper

instance ToSchema DocumentTemplateAssetDTO where
  declareNamedSchema = toSwagger (toDTO assetLogo "https://s3.com/asset-logo.png")
