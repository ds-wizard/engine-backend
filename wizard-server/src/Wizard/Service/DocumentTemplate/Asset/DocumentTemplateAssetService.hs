module Wizard.Service.DocumentTemplate.Asset.DocumentTemplateAssetService where

import Control.Monad.Reader (asks, liftIO)
import qualified Data.ByteString.Char8 as BS
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Util.Uuid
import Shared.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateAssetDAO
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import Wizard.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetChangeDTO
import Wizard.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetCreateDTO
import Wizard.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.S3.DocumentTemplate.DocumentTemplateS3
import Wizard.Service.DocumentTemplate.Asset.DocumentTemplateAssetMapper
import Wizard.Service.DocumentTemplate.DocumentTemplateValidation
import Wizard.Service.Tenant.Limit.LimitService

getAssets :: U.UUID -> AppContextM [DocumentTemplateAssetDTO]
getAssets dtUuid = do
  checkPermission _DOC_TML_WRITE_PERM
  assets <- findAssetsByDocumentTemplateUuid dtUuid
  now <- liftIO getCurrentTime
  traverse
    ( \asset -> do
        let expirationInSeconds = 60
        let urlExpiration = addUTCTime (realToFrac expirationInSeconds) now
        url <- presignGetAssetUrl asset.documentTemplateUuid asset.uuid expirationInSeconds
        return $ toDTO asset url urlExpiration
    )
    assets

getAsset :: U.UUID -> AppContextM DocumentTemplateAssetDTO
getAsset assetUuid = do
  checkPermission _DOC_TML_WRITE_PERM
  asset <- findAssetById assetUuid
  let expirationInSeconds = 60
  now <- liftIO getCurrentTime
  let urlExpiration = addUTCTime (realToFrac expirationInSeconds) now
  url <- presignGetAssetUrl asset.documentTemplateUuid asset.uuid expirationInSeconds
  return $ toDTO asset url urlExpiration

getAssetContent :: U.UUID -> U.UUID -> AppContextM (DocumentTemplateAsset, BS.ByteString)
getAssetContent dtUuid assetUuid = do
  asset <- findAssetById assetUuid
  content <- retrieveAsset dtUuid asset.uuid
  return (asset, content)

createAsset :: U.UUID -> DocumentTemplateAssetCreateDTO -> AppContextM DocumentTemplateAssetDTO
createAsset dtUuid reqDto =
  runInTransaction $ do
    checkPermission _DOC_TML_WRITE_PERM
    checkStorageSize (fromIntegral . BS.length $ reqDto.content)
    validateFileAndAssetUniqueness Nothing dtUuid reqDto.fileName
    aUuid <- liftIO generateUuid
    tenantUuid <- asks currentTenantUuid
    now <- liftIO getCurrentTime
    let fileSize = fromIntegral . BS.length $ reqDto.content
    let newAsset = fromCreateDTO dtUuid aUuid reqDto.fileName reqDto.contentType fileSize tenantUuid now now
    insertAsset newAsset
    touchDocumentTemplateByUuid newAsset.documentTemplateUuid
    putAsset dtUuid aUuid reqDto.contentType reqDto.content
    deleteTemporalDocumentsByAssetUuid aUuid
    let expirationInSeconds = 60
    now <- liftIO getCurrentTime
    let urlExpiration = addUTCTime (realToFrac expirationInSeconds) now
    url <- presignGetAssetUrl newAsset.documentTemplateUuid aUuid expirationInSeconds
    return $ toDTO newAsset url urlExpiration

modifyAsset :: U.UUID -> DocumentTemplateAssetChangeDTO -> AppContextM DocumentTemplateAsset
modifyAsset assetUuid reqDto =
  runInTransaction $ do
    checkPermission _DOC_TML_WRITE_PERM
    asset <- findAssetById assetUuid
    validateFileAndAssetUniqueness (Just asset.uuid) asset.documentTemplateUuid reqDto.fileName
    now <- liftIO getCurrentTime
    let updatedAsset = fromChangeDTO asset reqDto now
    updateAssetById updatedAsset
    touchDocumentTemplateByUuid asset.documentTemplateUuid
    deleteTemporalDocumentsByAssetUuid assetUuid
    return updatedAsset

modifyAssetContent :: U.UUID -> DocumentTemplateAssetCreateDTO -> AppContextM DocumentTemplateAsset
modifyAssetContent assetUuid reqDto =
  runInTransaction $ do
    checkPermission _DOC_TML_WRITE_PERM
    checkStorageSize (fromIntegral . BS.length $ reqDto.content)
    now <- liftIO getCurrentTime
    asset <- findAssetById assetUuid
    let fileSize = fromIntegral . BS.length $ reqDto.content
    let updatedAsset = fromChangeContentDTO asset reqDto.fileName reqDto.contentType fileSize now
    updateAssetById updatedAsset
    touchDocumentTemplateByUuid updatedAsset.documentTemplateUuid
    deleteTemporalDocumentsByAssetUuid assetUuid
    putAsset asset.documentTemplateUuid assetUuid reqDto.contentType reqDto.content
    return updatedAsset

duplicateAsset :: U.UUID -> DocumentTemplateAsset -> AppContextM DocumentTemplateAsset
duplicateAsset newDtUuid asset = do
  content <- retrieveAsset asset.documentTemplateUuid asset.uuid
  aUuid <- liftIO generateUuid
  now <- liftIO getCurrentTime
  let updatedAsset = fromDuplicateDTO asset newDtUuid aUuid now
  insertAsset updatedAsset
  touchDocumentTemplateByUuid asset.documentTemplateUuid
  putAsset newDtUuid aUuid updatedAsset.contentType content
  return updatedAsset

deleteAsset :: U.UUID -> U.UUID -> AppContextM ()
deleteAsset dtUuid assetUuid =
  runInTransaction $ do
    checkPermission _DOC_TML_WRITE_PERM
    asset <- findAssetById assetUuid
    deleteAssetById asset.uuid
    removeAsset dtUuid assetUuid
    touchDocumentTemplateByUuid dtUuid
    deleteTemporalDocumentsByAssetUuid assetUuid
    return ()
