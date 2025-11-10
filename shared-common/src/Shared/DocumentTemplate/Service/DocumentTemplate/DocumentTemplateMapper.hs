module Shared.DocumentTemplate.Service.DocumentTemplate.DocumentTemplateMapper where

import Data.Time
import qualified Data.UUID as U
import GHC.Int

import Shared.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateDTO
import Shared.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateSuggestionDTO
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateFormatSimple

toDTO :: DocumentTemplate -> [DocumentTemplateFormat] -> DocumentTemplateDTO
toDTO tml formats =
  DocumentTemplateDTO
    { tId = tml.tId
    , name = tml.name
    , version = tml.version
    , phase = tml.phase
    , description = tml.description
    , formats = fmap toFormatSimple formats
    }

toSuggestionDTO :: DocumentTemplate -> [DocumentTemplateFormat] -> DocumentTemplateSuggestionDTO
toSuggestionDTO tml formats =
  DocumentTemplateSuggestionDTO
    { tId = tml.tId
    , name = tml.name
    , version = tml.version
    , description = tml.description
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

fromFormatDTO :: String -> U.UUID -> UTCTime -> UTCTime -> DocumentTemplateFormatDTO -> DocumentTemplateFormat
fromFormatDTO documentTemplateId tenantUuid createdAt updatedAt format =
  DocumentTemplateFormat
    { documentTemplateId = documentTemplateId
    , uuid = format.uuid
    , name = format.name
    , icon = format.icon
    , steps = zipWith (\i s -> fromFormatStepDTO documentTemplateId format.uuid i tenantUuid createdAt updatedAt s) [0 ..] format.steps
    , tenantUuid = tenantUuid
    , createdAt = createdAt
    , updatedAt = updatedAt
    }

fromFormatStepDTO :: String -> U.UUID -> Int -> U.UUID -> UTCTime -> UTCTime -> DocumentTemplateFormatStepDTO -> DocumentTemplateFormatStep
fromFormatStepDTO documentTemplateId formatUuid position tenantUuid createdAt updatedAt step =
  DocumentTemplateFormatStep
    { documentTemplateId = documentTemplateId
    , formatUuid = formatUuid
    , position = position
    , name = step.name
    , options = step.options
    , tenantUuid = tenantUuid
    , createdAt = createdAt
    , updatedAt = updatedAt
    }

fromFileDTO :: String -> U.UUID -> UTCTime -> DocumentTemplateFileDTO -> DocumentTemplateFile
fromFileDTO documentTemplateId tenantUuid now file =
  DocumentTemplateFile
    { documentTemplateId = documentTemplateId
    , uuid = file.uuid
    , fileName = file.fileName
    , content = file.content
    , tenantUuid = tenantUuid
    , createdAt = now
    , updatedAt = now
    }

fromAssetDTO :: String -> Int64 -> U.UUID -> UTCTime -> DocumentTemplateAssetDTO -> DocumentTemplateAsset
fromAssetDTO documentTemplateId fileSize tenantUuid now asset =
  DocumentTemplateAsset
    { documentTemplateId = documentTemplateId
    , uuid = asset.uuid
    , fileName = asset.fileName
    , contentType = asset.contentType
    , fileSize = fileSize
    , tenantUuid = tenantUuid
    , createdAt = now
    , updatedAt = now
    }
