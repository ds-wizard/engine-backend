module Shared.Service.Template.TemplateMapper where

import qualified Data.UUID as U
import GHC.Int

import Shared.Api.Resource.Template.TemplateDTO
import Shared.Api.Resource.Template.TemplateFormatDTO
import Shared.Api.Resource.Template.TemplateSuggestionDTO
import Shared.Model.Template.Template
import Shared.Util.List

toDTO :: Template -> TemplateDTO
toDTO tml =
  TemplateDTO
    { tId = tml.tId
    , name = tml.name
    , version = tml.version
    , description = tml.description
    , formats = fmap toFormatDTO tml.formats
    }

toSuggestionDTO :: Template -> TemplateSuggestionDTO
toSuggestionDTO tml =
  TemplateSuggestionDTO
    { tId = tml.tId
    , name = tml.name
    , version = tml.version
    , description = tml.description
    , formats = fmap toFormatDTO tml.formats
    }

toFormatDTO :: TemplateFormat -> TemplateFormatDTO
toFormatDTO format =
  TemplateFormatDTO
    { uuid = format.uuid
    , name = format.name
    , shortName = format.shortName
    , icon = format.icon
    , color = format.color
    , isPdf =
        case lastSafe $ format.steps of
          Nothing -> False
          Just step -> step.name == "wkhtmltopdf"
    }

toFileDTO :: TemplateFile -> TemplateFileDTO
toFileDTO file =
  TemplateFileDTO
    { uuid = file.uuid
    , fileName = file.fileName
    , content = file.content
    }

toAssetDTO :: TemplateAsset -> TemplateAssetDTO
toAssetDTO asset =
  TemplateAssetDTO
    { uuid = asset.uuid
    , fileName = asset.fileName
    , contentType = asset.contentType
    }

fromFileDTO :: String -> U.UUID -> TemplateFileDTO -> TemplateFile
fromFileDTO templateId appUuid file =
  TemplateFile
    { templateId = templateId
    , uuid = file.uuid
    , fileName = file.fileName
    , content = file.content
    , appUuid = appUuid
    }

fromAssetDTO :: String -> Int64 -> U.UUID -> TemplateAssetDTO -> TemplateAsset
fromAssetDTO templateId fileSize appUuid asset =
  TemplateAsset
    { templateId = templateId
    , uuid = asset.uuid
    , fileName = asset.fileName
    , contentType = asset.contentType
    , fileSize = fileSize
    , appUuid = appUuid
    }
