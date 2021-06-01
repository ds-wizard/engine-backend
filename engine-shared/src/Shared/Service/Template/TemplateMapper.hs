module Shared.Service.Template.TemplateMapper where

import Control.Lens ((^.))

import LensesConfig
import Shared.Api.Resource.Template.TemplateDTO
import Shared.Api.Resource.Template.TemplateFormatDTO
import Shared.Api.Resource.Template.TemplateSuggestionDTO
import Shared.Model.Template.Template

toDTO :: Template -> TemplateDTO
toDTO tml =
  TemplateDTO
    { _templateDTOTId = tml ^. tId
    , _templateDTOName = tml ^. name
    , _templateDTOVersion = tml ^. version
    , _templateDTODescription = tml ^. description
    , _templateDTOFormats = fmap toFormatDTO (tml ^. formats)
    }

toSuggestionDTO :: Template -> TemplateSuggestionDTO
toSuggestionDTO tml =
  TemplateSuggestionDTO
    { _templateSuggestionDTOTId = tml ^. tId
    , _templateSuggestionDTOName = tml ^. name
    , _templateSuggestionDTOVersion = tml ^. version
    , _templateSuggestionDTODescription = tml ^. description
    , _templateSuggestionDTOFormats = fmap toFormatDTO (tml ^. formats)
    }

toFormatDTO :: TemplateFormat -> TemplateFormatDTO
toFormatDTO format =
  TemplateFormatDTO
    { _templateFormatDTOUuid = format ^. uuid
    , _templateFormatDTOName = format ^. name
    , _templateFormatDTOShortName = format ^. shortName
    , _templateFormatDTOIcon = format ^. icon
    , _templateFormatDTOColor = format ^. color
    }

toFileDTO :: TemplateFile -> TemplateFileDTO
toFileDTO file =
  TemplateFileDTO
    { _templateFileDTOUuid = file ^. uuid
    , _templateFileDTOFileName = file ^. fileName
    , _templateFileDTOContent = file ^. content
    }

toAssetDTO :: TemplateAsset -> TemplateAssetDTO
toAssetDTO asset =
  TemplateAssetDTO
    { _templateAssetDTOUuid = asset ^. uuid
    , _templateAssetDTOFileName = asset ^. fileName
    , _templateAssetDTOContentType = asset ^. contentType
    }

fromFileDTO :: String -> TemplateFileDTO -> TemplateFile
fromFileDTO templateId file =
  TemplateFile
    { _templateFileTemplateId = templateId
    , _templateFileUuid = file ^. uuid
    , _templateFileFileName = file ^. fileName
    , _templateFileContent = file ^. content
    }

fromAssetDTO :: String -> TemplateAssetDTO -> TemplateAsset
fromAssetDTO templateId asset =
  TemplateAsset
    { _templateAssetTemplateId = templateId
    , _templateAssetUuid = asset ^. uuid
    , _templateAssetFileName = asset ^. fileName
    , _templateAssetContentType = asset ^. contentType
    }
