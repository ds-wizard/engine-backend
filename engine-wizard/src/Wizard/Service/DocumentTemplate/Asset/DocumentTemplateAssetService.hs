module Wizard.Service.DocumentTemplate.Asset.DocumentTemplateAssetService where

import Control.Monad.Reader (asks, liftIO)
import qualified Data.ByteString.Char8 as BS
import Data.Time
import qualified Data.UUID as U

import Shared.Database.DAO.DocumentTemplate.DocumentTemplateAssetDAO
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Util.Uuid
import Wizard.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetChangeDTO
import Wizard.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetCreateDTO
import Wizard.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.S3.DocumentTemplate.DocumentTemplateS3
import Wizard.Service.Acl.AclService
import Wizard.Service.DocumentTemplate.Asset.DocumentTemplateAssetMapper
import Wizard.Service.DocumentTemplate.DocumentTemplateValidation
import Wizard.Service.Limit.AppLimitService

getAssets :: String -> AppContextM [DocumentTemplateAssetDTO]
getAssets tmlId = do
  checkPermission _DOC_TML_WRITE_PERM
  assets <- findAssetsByDocumentTemplateId tmlId
  traverse
    ( \asset -> do
        url <- makePublicLink asset.documentTemplateId asset.uuid
        return $ toDTO asset url
    )
    assets

getAsset :: U.UUID -> AppContextM DocumentTemplateAssetDTO
getAsset assetUuid = do
  checkPermission _DOC_TML_WRITE_PERM
  asset <- findAssetById assetUuid
  url <- makePublicLink asset.documentTemplateId assetUuid
  return $ toDTO asset url

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
    appUuid <- asks currentAppUuid
    now <- liftIO getCurrentTime
    let fileSize = fromIntegral . BS.length $ reqDto.content
    let newAsset = fromCreateDTO tmlId aUuid reqDto.fileName reqDto.contentType fileSize appUuid now now
    insertAsset newAsset
    putAsset tmlId aUuid reqDto.contentType reqDto.content
    deleteTemporalDocumentsByAssetUuid aUuid
    url <- makePublicLink newAsset.documentTemplateId aUuid
    return $ toDTO newAsset url

modifyAsset :: U.UUID -> DocumentTemplateAssetChangeDTO -> AppContextM DocumentTemplateAsset
modifyAsset assetUuid reqDto =
  runInTransaction $ do
    checkPermission _DOC_TML_WRITE_PERM
    asset <- findAssetById assetUuid
    validateFileAndAssetUniqueness (Just asset.uuid) asset.documentTemplateId reqDto.fileName
    now <- liftIO getCurrentTime
    let updatedAsset = fromChangeDTO asset reqDto now
    updateAssetById updatedAsset
    deleteTemporalDocumentsByFileUuid assetUuid
    return updatedAsset

modifyAssetContent :: U.UUID -> DocumentTemplateAssetCreateDTO -> AppContextM DocumentTemplateAsset
modifyAssetContent assetUuid reqDto =
  runInTransaction $ do
    checkPermission _DOC_TML_WRITE_PERM
    checkStorageSize (fromIntegral . BS.length $ reqDto.content)
    now <- liftIO getCurrentTime
    asset <- findAssetById assetUuid
    let fileSize = fromIntegral . BS.length $ reqDto.content
    now <- liftIO getCurrentTime
    let updatedAsset = fromChangeContentDTO asset reqDto.fileName reqDto.contentType fileSize now
    updateAssetById updatedAsset
    deleteTemporalDocumentsByFileUuid assetUuid
    putAsset asset.documentTemplateId assetUuid reqDto.contentType reqDto.content
    return updatedAsset

duplicateAsset :: String -> DocumentTemplateAsset -> AppContextM DocumentTemplateAsset
duplicateAsset newTemplateId asset = do
  content <- retrieveAsset asset.documentTemplateId asset.uuid
  aUuid <- liftIO generateUuid
  now <- liftIO getCurrentTime
  let updatedAsset = fromDuplicateDTO asset newTemplateId aUuid now
  insertAsset updatedAsset
  putAsset newTemplateId aUuid updatedAsset.contentType content
  return updatedAsset

deleteAsset :: String -> U.UUID -> AppContextM ()
deleteAsset tmlId assetUuid =
  runInTransaction $ do
    checkPermission _DOC_TML_WRITE_PERM
    asset <- findAssetById assetUuid
    deleteAssetById asset.uuid
    removeAsset tmlId assetUuid
    deleteTemporalDocumentsByAssetUuid assetUuid
    return ()
