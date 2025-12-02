module Wizard.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateAssets where

import Shared.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateAssets
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import Wizard.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetChangeDTO

assetLogoChangeDTO :: DocumentTemplateAssetChangeDTO
assetLogoChangeDTO =
  DocumentTemplateAssetChangeDTO
    { fileName = assetLogoEdited.fileName
    }
