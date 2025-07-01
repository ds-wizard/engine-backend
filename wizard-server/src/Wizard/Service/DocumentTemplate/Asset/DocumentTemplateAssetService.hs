module Wizard.Service.DocumentTemplate.Asset.DocumentTemplateAssetService where

import Control.Monad.Reader (asks, liftIO)
import qualified Data.ByteString.Char8 as BS
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Util.Uuid
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
import WizardLib.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateAssetDAO
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate

getAssets :: String -> AppContextM [DocumentTemplateAssetDTO]
getAssets tmlId = do
  checkPermission _DOC_TML_WRITE_PERM
  assets <- findAssetsByDocumentTemplateId tmlId
  now <- liftIO getCurrentTime
  traverse
    ( \asset -> do
        let expirationInSeconds = 60
        let urlExpiration = addUTCTime (realToFrac expirationInSeconds) now
        url <- presigneGetAssetUrl asset.documentTemplateId asset.uuid expirationInSeconds
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
  url <- presigneGetAssetUrl asset.documentTemplateId asset.uuid expirationInSeconds
  return $ toDTO asset url urlExpiration

getAssetContent :: String -> U.UUID -> AppContextM (DocumentTemplateAsset, BS.ByteString)
getAssetContent tmlId assetUuid = do
  asset <- findAssetById assetUuid
  content <- retrieveAsset tmlId asset.uuid
  return (asset, content)

createAsset :: String -> DocumentTemplateAssetCreateDTO -> AppContextM DocumentTemplateAssetDTO
createAsset tmlId reqDto =
  runInTransaction $ do
    checkPermission _DOC_TML_WRITE_PERM
    checkStorageSize (fromIntegral . BS.length $ reqDto.content)
    validateFileAndAssetUniqueness Nothing tmlId reqDto.fileName
    aUuid <- liftIO generateUuid
    tenantUuid <- asks currentTenantUuid
    now <- liftIO getCurrentTime
    let fileSize = fromIntegral . BS.length $ reqDto.content
    let newAsset = fromCreateDTO tmlId aUuid reqDto.fileName reqDto.contentType fileSize tenantUuid now now
    insertAsset newAsset
    touchDocumentTemplateById newAsset.documentTemplateId
    putAsset tmlId aUuid reqDto.contentType reqDto.content
    deleteTemporalDocumentsByAssetUuid aUuid
    let expirationInSeconds = 60
    now <- liftIO getCurrentTime
    let urlExpiration = addUTCTime (realToFrac expirationInSeconds) now
    url <- presigneGetAssetUrl newAsset.documentTemplateId aUuid expirationInSeconds
    return $ toDTO newAsset url urlExpiration

modifyAsset :: U.UUID -> DocumentTemplateAssetChangeDTO -> AppContextM DocumentTemplateAsset
modifyAsset assetUuid reqDto =
  runInTransaction $ do
    checkPermission _DOC_TML_WRITE_PERM
    asset <- findAssetById assetUuid
    validateFileAndAssetUniqueness (Just asset.uuid) asset.documentTemplateId reqDto.fileName
    now <- liftIO getCurrentTime
    let updatedAsset = fromChangeDTO asset reqDto now
    updateAssetById updatedAsset
    touchDocumentTemplateById asset.documentTemplateId
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
    touchDocumentTemplateById updatedAsset.documentTemplateId
    deleteTemporalDocumentsByAssetUuid assetUuid
    putAsset asset.documentTemplateId assetUuid reqDto.contentType reqDto.content
    return updatedAsset

duplicateAsset :: String -> DocumentTemplateAsset -> AppContextM DocumentTemplateAsset
duplicateAsset newTemplateId asset = do
  content <- retrieveAsset asset.documentTemplateId asset.uuid
  aUuid <- liftIO generateUuid
  now <- liftIO getCurrentTime
  let updatedAsset = fromDuplicateDTO asset newTemplateId aUuid now
  insertAsset updatedAsset
  touchDocumentTemplateById asset.documentTemplateId
  putAsset newTemplateId aUuid updatedAsset.contentType content
  return updatedAsset

deleteAsset :: String -> U.UUID -> AppContextM ()
deleteAsset tmlId assetUuid =
  runInTransaction $ do
    checkPermission _DOC_TML_WRITE_PERM
    asset <- findAssetById assetUuid
    deleteAssetById asset.uuid
    removeAsset tmlId assetUuid
    touchDocumentTemplateById tmlId
    deleteTemporalDocumentsByAssetUuid assetUuid
    return ()
