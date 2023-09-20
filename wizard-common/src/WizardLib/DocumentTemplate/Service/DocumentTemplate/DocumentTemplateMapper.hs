module WizardLib.DocumentTemplate.Service.DocumentTemplate.DocumentTemplateMapper where

import Data.Time
import qualified Data.UUID as U
import GHC.Int

import Shared.Common.Util.List
import WizardLib.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateDTO
import WizardLib.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateFormatDTO
import WizardLib.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateSuggestionDTO
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate

toDTO :: DocumentTemplate -> DocumentTemplateDTO
toDTO tml =
  DocumentTemplateDTO
    { tId = tml.tId
    , name = tml.name
    , version = tml.version
    , phase = tml.phase
    , description = tml.description
    , formats = fmap toFormatDTO tml.formats
    }

toSuggestionDTO :: DocumentTemplate -> DocumentTemplateSuggestionDTO
toSuggestionDTO tml =
  DocumentTemplateSuggestionDTO
    { tId = tml.tId
    , name = tml.name
    , version = tml.version
    , description = tml.description
    , formats = fmap toFormatDTO tml.formats
    }

toFormatDTO :: DocumentTemplateFormat -> DocumentTemplateFormatDTO
toFormatDTO format =
  DocumentTemplateFormatDTO
    { uuid = format.uuid
    , name = format.name
    , icon = format.icon
    , isPdf =
        case lastSafe $ format.steps of
          Nothing -> False
          Just step -> step.name == "wkhtmltopdf" || step.name == "weasyprint"
    }

toFileDTO :: DocumentTemplateFile -> DocumentTemplateFileDTO
toFileDTO file =
  DocumentTemplateFileDTO
    { uuid = file.uuid
    , fileName = file.fileName
    , content = file.content
    }

toAssetDTO :: DocumentTemplateAsset -> DocumentTemplateAssetDTO
toAssetDTO asset =
  DocumentTemplateAssetDTO
    { uuid = asset.uuid
    , fileName = asset.fileName
    , contentType = asset.contentType
    }

fromFileDTO :: String -> U.UUID -> UTCTime -> DocumentTemplateFileDTO -> DocumentTemplateFile
fromFileDTO documentTemplateId appUuid now file =
  DocumentTemplateFile
    { documentTemplateId = documentTemplateId
    , uuid = file.uuid
    , fileName = file.fileName
    , content = file.content
    , appUuid = appUuid
    , createdAt = now
    , updatedAt = now
    }

fromAssetDTO :: String -> Int64 -> U.UUID -> UTCTime -> DocumentTemplateAssetDTO -> DocumentTemplateAsset
fromAssetDTO documentTemplateId fileSize appUuid now asset =
  DocumentTemplateAsset
    { documentTemplateId = documentTemplateId
    , uuid = asset.uuid
    , fileName = asset.fileName
    , contentType = asset.contentType
    , fileSize = fileSize
    , appUuid = appUuid
    , createdAt = now
    , updatedAt = now
    }
