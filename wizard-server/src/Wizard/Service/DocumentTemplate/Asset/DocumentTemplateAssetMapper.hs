module Wizard.Service.DocumentTemplate.Asset.DocumentTemplateAssetMapper where

import Data.Time
import qualified Data.UUID as U
import GHC.Int

import Wizard.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetChangeDTO
import Wizard.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetDTO
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate

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

fromCreateDTO :: String -> U.UUID -> String -> String -> Int64 -> U.UUID -> UTCTime -> UTCTime -> DocumentTemplateAsset
fromCreateDTO tmlId aUuid fileName contentType fileSize appUuid createdAt updatedAt =
  DocumentTemplateAsset
    { documentTemplateId = tmlId
    , uuid = aUuid
    , fileName = fileName
    , contentType = contentType
    , fileSize = fileSize
    , appUuid = appUuid
    , createdAt = createdAt
    , updatedAt = updatedAt
    }

fromChangeDTO :: DocumentTemplateAsset -> DocumentTemplateAssetChangeDTO -> UTCTime -> DocumentTemplateAsset
fromChangeDTO asset reqDto now =
  DocumentTemplateAsset
    { documentTemplateId = asset.documentTemplateId
    , uuid = asset.uuid
    , fileName = reqDto.fileName
    , contentType = asset.contentType
    , fileSize = asset.fileSize
    , appUuid = asset.appUuid
    , createdAt = asset.createdAt
    , updatedAt = now
    }

fromChangeContentDTO :: DocumentTemplateAsset -> String -> String -> Int64 -> UTCTime -> DocumentTemplateAsset
fromChangeContentDTO asset fileName contentType fileSize now =
  DocumentTemplateAsset
    { documentTemplateId = asset.documentTemplateId
    , uuid = asset.uuid
    , fileName = fileName
    , contentType = contentType
    , fileSize = fileSize
    , appUuid = asset.appUuid
    , createdAt = asset.createdAt
    , updatedAt = now
    }

fromDuplicateDTO :: DocumentTemplateAsset -> String -> U.UUID -> UTCTime -> DocumentTemplateAsset
fromDuplicateDTO asset documentTemplateId uuid now =
  DocumentTemplateAsset
    { documentTemplateId = documentTemplateId
    , uuid = uuid
    , fileName = asset.fileName
    , contentType = asset.contentType
    , fileSize = asset.fileSize
    , appUuid = asset.appUuid
    , createdAt = now
    , updatedAt = now
    }
