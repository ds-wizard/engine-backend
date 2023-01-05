module Wizard.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateAssets where

import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateAssets
import Shared.Model.DocumentTemplate.DocumentTemplate
import Wizard.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetChangeDTO

assetLogoChangeDTO :: DocumentTemplateAssetChangeDTO
assetLogoChangeDTO =
  DocumentTemplateAssetChangeDTO
    { fileName = assetLogoEdited.fileName
    }
