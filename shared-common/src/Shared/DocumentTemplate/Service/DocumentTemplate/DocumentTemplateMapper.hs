module Shared.DocumentTemplate.Service.DocumentTemplate.DocumentTemplateMapper where

import Data.Time
import qualified Data.UUID as U
import GHC.Int

import Shared.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateDTO
import Shared.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateSuggestionDTO
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateFormatSimple
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateSimple

toDTO :: DocumentTemplate -> [DocumentTemplateFormat] -> DocumentTemplateDTO
toDTO dt formats =
  DocumentTemplateDTO
    { uuid = dt.uuid
    , name = dt.name
    , version = dt.version
    , phase = dt.phase
    , description = dt.description
    , formats = fmap toFormatSimple formats
    }

toSimple :: DocumentTemplate -> DocumentTemplateSimple
toSimple DocumentTemplate {..} = DocumentTemplateSimple {..}

toSuggestionDTO :: DocumentTemplate -> [DocumentTemplateFormat] -> DocumentTemplateSuggestionDTO
toSuggestionDTO dt formats =
  DocumentTemplateSuggestionDTO
    { uuid = dt.uuid
    , name = dt.name
    , organizationId = dt.organizationId
    , templateId = dt.templateId
    , version = dt.version
    , description = dt.description
    , formats = fmap toFormatSimple formats
    }

toFormatSimple :: DocumentTemplateFormat -> DocumentTemplateFormatSimple
toFormatSimple DocumentTemplateFormat {..} = DocumentTemplateFormatSimple {..}

toFormatDTO :: DocumentTemplateFormat -> DocumentTemplateFormatDTO
toFormatDTO format =
  DocumentTemplateFormatDTO
    { uuid = format.uuid
    , name = format.name
    , icon = format.icon
    , steps = fmap toFormatStepDTO format.steps
    }

toFormatStepDTO :: DocumentTemplateFormatStep -> DocumentTemplateFormatStepDTO
toFormatStepDTO step =
  DocumentTemplateFormatStepDTO
    { name = step.name
    , options = step.options
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

fromFormatDTO :: U.UUID -> U.UUID -> UTCTime -> UTCTime -> DocumentTemplateFormatDTO -> DocumentTemplateFormat
fromFormatDTO documentTemplateUuid tenantUuid createdAt updatedAt format =
  DocumentTemplateFormat
    { documentTemplateUuid = documentTemplateUuid
    , uuid = format.uuid
    , name = format.name
    , icon = format.icon
    , steps = zipWith (\i s -> fromFormatStepDTO documentTemplateUuid format.uuid i tenantUuid createdAt updatedAt s) [0 ..] format.steps
    , tenantUuid = tenantUuid
    , createdAt = createdAt
    , updatedAt = updatedAt
    }

fromFormatStepDTO :: U.UUID -> U.UUID -> Int -> U.UUID -> UTCTime -> UTCTime -> DocumentTemplateFormatStepDTO -> DocumentTemplateFormatStep
fromFormatStepDTO documentTemplateUuid formatUuid position tenantUuid createdAt updatedAt step =
  DocumentTemplateFormatStep
    { documentTemplateUuid = documentTemplateUuid
    , formatUuid = formatUuid
    , position = position
    , name = step.name
    , options = step.options
    , tenantUuid = tenantUuid
    , createdAt = createdAt
    , updatedAt = updatedAt
    }

fromFileDTO :: U.UUID -> U.UUID -> UTCTime -> DocumentTemplateFileDTO -> DocumentTemplateFile
fromFileDTO documentTemplateUuid tenantUuid now file =
  DocumentTemplateFile
    { documentTemplateUuid = documentTemplateUuid
    , uuid = file.uuid
    , fileName = file.fileName
    , content = file.content
    , tenantUuid = tenantUuid
    , createdAt = now
    , updatedAt = now
    }

fromAssetDTO :: U.UUID -> Int64 -> U.UUID -> UTCTime -> DocumentTemplateAssetDTO -> DocumentTemplateAsset
fromAssetDTO documentTemplateUuid fileSize tenantUuid now asset =
  DocumentTemplateAsset
    { documentTemplateUuid = documentTemplateUuid
    , uuid = asset.uuid
    , fileName = asset.fileName
    , contentType = asset.contentType
    , fileSize = fileSize
    , tenantUuid = tenantUuid
    , createdAt = now
    , updatedAt = now
    }
