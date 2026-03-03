module Wizard.Service.DocumentTemplate.Asset.DocumentTemplateAssetMapper where

import Data.Time
import qualified Data.UUID as U
import GHC.Int

import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import Wizard.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetChangeDTO
import Wizard.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetDTO

toDTO :: DocumentTemplateAsset -> String -> UTCTime -> DocumentTemplateAssetDTO
toDTO asset url urlExpiration =
  DocumentTemplateAssetDTO
    { uuid = asset.uuid
    , fileName = asset.fileName
    , contentType = asset.contentType
    , fileSize = asset.fileSize
    , url = url
    , urlExpiration = urlExpiration
    , createdAt = asset.createdAt
    , updatedAt = asset.updatedAt
    }

fromCreateDTO :: U.UUID -> U.UUID -> String -> String -> Int64 -> U.UUID -> UTCTime -> UTCTime -> DocumentTemplateAsset
fromCreateDTO dtUuid aUuid fileName contentType fileSize tenantUuid createdAt updatedAt =
  DocumentTemplateAsset
    { documentTemplateUuid = dtUuid
    , uuid = aUuid
    , fileName = fileName
    , contentType = contentType
    , fileSize = fileSize
    , tenantUuid = tenantUuid
    , createdAt = createdAt
    , updatedAt = updatedAt
    }

fromChangeDTO :: DocumentTemplateAsset -> DocumentTemplateAssetChangeDTO -> UTCTime -> DocumentTemplateAsset
fromChangeDTO asset reqDto now =
  DocumentTemplateAsset
    { documentTemplateUuid = asset.documentTemplateUuid
    , uuid = asset.uuid
    , fileName = reqDto.fileName
    , contentType = asset.contentType
    , fileSize = asset.fileSize
    , tenantUuid = asset.tenantUuid
    , createdAt = asset.createdAt
    , updatedAt = now
    }

fromChangeContentDTO :: DocumentTemplateAsset -> String -> String -> Int64 -> UTCTime -> DocumentTemplateAsset
fromChangeContentDTO asset fileName contentType fileSize now =
  DocumentTemplateAsset
    { documentTemplateUuid = asset.documentTemplateUuid
    , uuid = asset.uuid
    , fileName = fileName
    , contentType = contentType
    , fileSize = fileSize
    , tenantUuid = asset.tenantUuid
    , createdAt = asset.createdAt
    , updatedAt = now
    }

fromDuplicateDTO :: DocumentTemplateAsset -> U.UUID -> U.UUID -> UTCTime -> DocumentTemplateAsset
fromDuplicateDTO asset documentTemplateUuid uuid now =
  DocumentTemplateAsset
    { documentTemplateUuid = documentTemplateUuid
    , uuid = uuid
    , fileName = asset.fileName
    , contentType = asset.contentType
    , fileSize = asset.fileSize
    , tenantUuid = asset.tenantUuid
    , createdAt = now
    , updatedAt = now
    }
