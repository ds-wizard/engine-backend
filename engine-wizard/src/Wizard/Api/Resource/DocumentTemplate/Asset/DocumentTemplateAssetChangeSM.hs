module Wizard.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetChangeSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetChangeDTO
import Wizard.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetChangeJM ()
import Wizard.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateAssets

instance ToSchema DocumentTemplateAssetChangeDTO where
  declareNamedSchema = toSwagger assetLogoChangeDTO
