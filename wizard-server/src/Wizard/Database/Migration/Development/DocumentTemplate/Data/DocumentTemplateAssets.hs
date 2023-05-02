module Wizard.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateAssets where

import Wizard.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetChangeDTO
import WizardLib.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateAssets
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate

assetLogoChangeDTO :: DocumentTemplateAssetChangeDTO
assetLogoChangeDTO =
  DocumentTemplateAssetChangeDTO
    { fileName = assetLogoEdited.fileName
    }
